%%%-------------------------------------------------------------------
%%% @author Anna Nosek
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2020 11:26 PM
%%%-------------------------------------------------------------------
-module(pollution).
-author("anian").

%% API
-export([createMonitor/0, addStation/2, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, daysOverLimitInMonth/6, biggestAvgInMonth/4]).


-record(monitor, {stations, values}).
-record(value, {name, datetime, type, value}).

createMonitor() -> #monitor{stations = dict:new(), values = [] }.         %%creates an empty monitor


doesStationExist([], _) -> false;
doesStationExist([{_, Name}|_], Name) -> true;
doesStationExist([_|T], Name)-> doesStationExist(T, Name);
doesStationExist(StationsDict, Station) when not is_list(StationsDict)->    %%takes dictionary of stations and Station (name or coordinates)
  Res = dict:find(Station, StationsDict),                                   %%returns true if station exists in given dictionary, false otherwise
  case Res of
    {ok, _} -> true;
    error -> doesStationExist(dict:to_list(StationsDict), Station)
  end.


addStation(M, _) when not is_record(M, monitor) -> {error, notAMonitor};    %adds a new station to a monitor
addStation(#monitor{stations = S, values = V}, {N, {X, Y}}) ->
  IsString = io_lib:latin1_char_list(N),
  IsTaken = doesStationExist(S, N) or doesStationExist(S, {X, Y}),
  if
    not IsTaken and IsString -> #monitor{stations=dict:store({X,Y}, N, S), values=V};
    IsTaken -> {error, alreadyExists};
    true -> {error, invalidEntry}
  end;
addStation(_,_) -> {error, invalidEntry}.



isInValList([], _) -> false;                                    %%checks if given value record in already present in list of values
isInValList([#value{name = Name, datetime = DateTime, type=Type}|T], Entry) ->
  case Entry of
    #value{name = Name, datetime = DateTime, type=Type} -> true;
    _ -> isInValList(T, Entry)
  end.



getName(StationsDict, Station) ->                               %%takes dictionary of stations and a station (name of coordinates)
  case Station of                                               %%returns name of the station
    {_, _} -> case dict:find(Station, StationsDict) of
                {ok, Value} -> Value;
                error -> {error, noEntries}
              end;
    _ -> Station
  end.



addValue(M, _, _, _, Val) when not is_record(M, monitor) and not is_number(Val) ->    %%adds new value to a monitor if all the data is proper
  {error, invalidData};                                                               %%returns error otherwise
addValue(M, Station, {Date, Time}, Type, Val) ->
  IsDateValid = calendar:valid_date(Date),
  IsTypeValid = io_lib:latin1_char_list(Type),
  StationExists = doesStationExist(M#monitor.stations, Station),
  if
    IsDateValid and IsTypeValid ->
      Name = getName(M#monitor.stations, Station),
      Entry = #value{name = Name, datetime = {Date, Time}, type = Type, value = Val},
      IsNewEntry = not isInValList(M#monitor.values, Entry),
      if
          not StationExists -> {error, noSuchStation};
          IsNewEntry -> #monitor{stations=M#monitor.stations, values=[Entry|M#monitor.values]};
        true -> {error, alreadyExists}
      end;
    true -> {error, invalidData}
  end.



removeFromVals([], _, Res) -> Res;                                    %%removes a value record from a list of values
removeFromVals([#value{name = N, datetime = DT, type = T}|Ta], #value{name = N, datetime = DT, type = T}, Res) -> removeFromVals(Ta, #value{name = N, datetime = DT, type = T}, Res);
removeFromVals([H|T], Entry, Res) -> removeFromVals(T, Entry, [H|Res]).


removeValue(M, Station, DateTime, Type) when is_record(M, monitor) ->   %%removes a value from a valid monitor
  Stations = M#monitor.stations,
  Values = M#monitor.values,
  Entry = #value{name = getName(Stations, Station), datetime = DateTime, type = Type},
  #monitor{stations=Stations, values=lists:reverse(removeFromVals(Values, Entry, []))};
removeValue(_,_,_,_) -> {error, invalidValues}.



searchForValue([], _) -> {error, noEntries};                      %%finds value in list of values
searchForValue([#value{name = N, datetime = DT, type = T, value = V}|_], #value{name = N, datetime = DT, type = T}) -> V;
searchForValue([_|T], Entry) -> searchForValue(T, Entry).


getOneValue(M, Station, DateTime, Type) when is_record(M, monitor) ->     %%finds a value of given station, datetime and type in a monitor
  Stations = M#monitor.stations,
  Values = M#monitor.values,
  searchForValue(Values, #value{ name = getName(Stations, Station), datetime = DateTime, type = Type, value = 1});
getOneValue(_, _, _, _) -> {error, invalidValues}.



getStationSumList([], _, _, Sum, Num) -> {Sum, Num};                          %%calculates sum and number of values of given type in given station from a list of values
getStationSumList([#value{name = Name, type = Type, value = Val}|T], Name, Type, Sum, Num) ->
  getStationSumList(T, Name, Type, Sum + Val, Num + 1);
getStationSumList([_|T], Name, Type, Sum, Num) ->
  getStationSumList(T, Name, Type, Sum, Num).


getStationMean(M, Station, Type) when is_record(M, monitor) ->        %%calculates mean value of given type on a given station
  Name = getName(M#monitor.stations, Station),
  case Name of
    {error, _} -> {error, invalidStation};
    _ -> {Sum, Num} = getStationSumList(M#monitor.values, Name, Type, 0, 0),
      case Num of
        0 -> {error, noEntries};
        _ -> Sum/Num
      end
  end;
getStationMean(_, _, _) -> {error, invalidData}.



getDayTypeList([], _, _, Acc) ->                      %%returns a list of values of given date and type
  Acc;
getDayTypeList([{value, Name, {Date, Time}, Type, Val}|T], Date, Type, Acc) ->
  getDayTypeList(T, Date, Type, [{value, Name, {Date, Time}, Type, Val}|Acc]);
getDayTypeList([_|T], Date, Type, Acc) ->
  getDayTypeList(T, Date, Type, Acc).


getDayMeanList(Values, {Year, Month, Day}, Type) ->     %%calculates average value of given type in given day from a list of values
  DayTypeList = getDayTypeList(Values, {Year, Month, Day}, Type, []),
  Sum = lists:foldl(fun(#value{value = Val}, Sum) -> Val + Sum end, 0, DayTypeList),
  Num = lists:flatlength(DayTypeList),
  case Num of
      0 -> {error, noEntries};
      _ -> Sum/Num
  end.


getDailyMean(M, Date, Type) ->      %%returns average value of given type in given day from a monitor
  getDayMeanList(M#monitor.values, Date, Type).



countDays([], _, _, _, _, _, Count, CountAll) -> {Count, CountAll};     %%calculates how many days in given month has the limit has been reached on given station from a list of values
countDays([{value, Name, { {Year, Month, _}, _ }, Type, Val}|T], Name, Year, Month, Limit, Type, Count, CountAll) ->
  if
    Val>Limit -> countDays(T, Name, Year, Month, Limit, Type, Count + 1, CountAll + 1);
    true -> countDays(T, Name, Year, Month, Limit, Type, Count, CountAll + 1)
  end;
countDays([_|T], Name, Year, Month, Limit, Type, Count, CountAll) ->
  countDays(T, Name, Year, Month, Limit, Type, Count, CountAll).


daysOverLimitInMonth(M, Station, Year, Month, Limit, Type) when is_record(M, monitor) ->   %%how many days in given month has the limit has been reached on given station
  Name = getName(M#monitor.stations, Station),
  case Name of
    {error, _} -> {error, invalidStation};
    _ ->  case countDays(M#monitor.values, Name, Year, Month, Limit, Type, 0, 0) of
            {_, 0} -> {error, noEntries};
            {Count, _} -> Count
          end
  end;
daysOverLimitInMonth(_, _, _, _, _, _) -> {error, invalidData}.



getBiggestAvg(Values, {Year, Month, Day}, MaxDay, MaxVal, Type) -> %% returns day with biggest average of given parameter in year from a list of values
  LastDay = calendar:last_day_of_the_month(Year, Month),          %% result is in a tuple {Day with biggest average, Biggest average}
  CurrVal = getDayMeanList(Values, {Year, Month, Day}, Type),
  if
    Day > LastDay -> {MaxDay, MaxVal};
    CurrVal == {error, noEntries} -> getBiggestAvg(Values, {Year, Month, Day+1}, MaxDay, MaxVal, Type);
    CurrVal>MaxVal -> getBiggestAvg(Values, {Year, Month, Day+1}, Day, CurrVal, Type);
    true -> getBiggestAvg(Values, {Year, Month, Day+1}, MaxDay, MaxVal, Type)
  end.


biggestAvgInMonth(M, Year, Month, Type) when is_record(M, monitor) ->   %%day with biggest average of given parameter in year
  BiggestAvg = getBiggestAvg(M#monitor.values, {Year, Month, 1}, -1, -100, Type),
  case BiggestAvg of
    {-1, _} -> {error, noEntries};
    _ -> BiggestAvg
  end;
biggestAvgInMonth(_, _, _, _) -> {error, invalidData}.