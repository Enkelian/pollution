-module(pollution_gen_server).
-behaviour(gen_server).

%% API
-export([start/0, stop/0, crash/0, addStation/2, addValue/4, getOneValue/3, removeValue/3, getStationMean/2, getDailyMean/2, daysOverLimitInMonth/5, biggestAvgInMonth/3]).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([])     -> {ok, pollution:createMonitor()}.



%% USER INTERFACE

start()       -> start_link().

stop()        -> gen_server:cast(?MODULE, stop).

crash()       -> gen_server:cast(?MODULE, crash).

addStation(Name, Coords)  ->
  gen_server:call(?MODULE, {addStat, Name, Coords}).

addValue(Station, DateTime, Type, Val) ->
  gen_server:call(?MODULE, {addVal, Station, DateTime, Type, Val}).

getOneValue(Station, DateTime, Type) ->
  gen_server:call(?MODULE, {getVal, Station, DateTime, Type}).

removeValue(Station, DateTime, Type) ->
  gen_server:call(?MODULE, {remVal, Station, DateTime, Type}).

getStationMean(Station, Type) ->
  gen_server:call(?MODULE, {getStatMean, Station, Type}).

getDailyMean(Date, Type) ->
  gen_server:call(?MODULE, {getDayMean, Date, Type}).

daysOverLimitInMonth(Station, Year, Month, Limit, Type) ->
  gen_server:call(?MODULE, {getDaysOverLimit, Station, Year, Month, Limit, Type}).

biggestAvgInMonth(Year, Month, Type) ->
  gen_server:call(?MODULE, {getBiggestAvg, Year, Month, Type}).



%% CALLBACKS



handle_cast(stop, Monitor) ->
  {stop, normal, Monitor};

handle_cast(crush, Monitor) ->
  no:exist(), {noreply, Monitor}.



handle_call({addStat, Name, Coords}, _From, Monitor) ->
  NewMonitor = pollution:addStation(Monitor, {Name, Coords}),
  case NewMonitor of
    {error, alreadyExists} -> {reply, {error, alreadyExists}, Monitor};
    {error, _} -> crash();
    NewMonitor -> {reply, {ok, "Monitor updated"}, NewMonitor}
  end;

handle_call({addVal, Station, DateTime, Type, Val}, _From, Monitor) ->
  NewMonitor = pollution:addValue(Monitor, Station, DateTime, Type, Val),
  case NewMonitor of
    {error, alreadyExists} -> {reply, {error, alreadyExists}, Monitor};
    {error, _} -> crash();
    NewMonitor -> {reply, {ok, "Monitor updated"}, NewMonitor}
  end;

handle_call({getVal, Station, DateTime, Type}, _From, Monitor) ->
  {reply, pollution:getOneValue(Monitor, Station, DateTime, Type), Monitor};

handle_call({remVal, Station, DateTime, Type}, _From, Monitor) ->
  NewMonitor = pollution:removeValue(Monitor, Station, DateTime, Type),
  case NewMonitor of
    {error, ErrorMsg} -> {reply, {error, ErrorMsg}, Monitor};
    NewMonitor -> {reply, {ok, "Monitor updated"}, NewMonitor}
  end;

handle_call({getStatMean, Station, Type}, _From, Monitor) ->
  {reply, pollution:getStationMean(Monitor, Station, Type), Monitor};

handle_call({getDayMean, Day, Type}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Monitor, Day, Type), Monitor};

handle_call({getDaysOverLimit, Station, Year, Month, Limit, Type}, _From, Monitor) ->
  {reply, pollution:daysOverLimitInMonth(Monitor,  Station, Year, Month, Limit, Type), Monitor};

handle_call({getBiggestAvg, Year, Month, Type}, _From, Monitor) ->
  {reply, pollution:biggestAvgInMonth(Monitor,  Year, Month, Type), Monitor}.


handle_info(_Info, State) -> {noreply, State}.


code_change(_OldVsn, State, _Extra) -> {ok, State}.


terminate(Reason, _) ->
  Reason.
