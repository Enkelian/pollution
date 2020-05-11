-module(pollution_gen_server_test).
-author("anian").

-include_lib("eunit/include/eunit.hrl").
-import(pollution_gen_server, [start/0, init/0, stop/0, getMonitor/0, addValue/4, addStation/2, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, daysOverLimitInMonth/5, biggestAvgInMonth/3]).


stopServer(_) ->
  stop().

startServer() ->
  start().


adding_test_() ->
  {
    "Tries adding existing and invalid stations and values.",
    {
      setup,
      fun startServer/0,
      fun stopServer/1,
      fun (_) ->
        {
          inorder,
          [
            test_addValidStations(),
            test_addValidValues(),
            test_addExistingStations(),
            test_addExistingValue(),
            test_getExistingValue(),
            test_getNotExistingValue(),
            test_removeValue(),
            test_getStats()
          ]
        }
      end
    }
  }.

test_addValidStations() ->
  [
    ?_assertEqual({ok, "Monitor updated"}, addStation("Turku", {123.3,12.3})),
    ?_assertEqual({ok, "Monitor updated"}, addStation("Helsinki", {333,222}))
  ].

test_addValidValues() ->
  [
    ?_assertEqual({ok, "Monitor updated"}, addValue("Helsinki", {{2020,3,30},{0,49,1}}, "PM10", 111)),
    ?_assertEqual({ok, "Monitor updated"}, addValue({333,222}, {{2020,3,30},{0,8,1}}, "PM10", 123)),
    ?_assertEqual({ok, "Monitor updated"}, addValue("Turku", {{2020,3,30},{0,9,1}}, "temperature", 15))
  ].

test_addExistingStations() ->
  [
    ?_assertEqual({error, alreadyExists}, addStation("Turku", {123.3,12.3})),
    ?_assertEqual({error, alreadyExists}, addStation("Helsinki", {123.3,12.3}))
  ].

test_addExistingValue() ->
  ?_assertEqual({error, alreadyExists}, addValue({333,222}, {{2020,3,30},{0,49,1}}, "PM10", 111)).

test_getExistingValue() -> ?_assertEqual(111, getOneValue("Helsinki", {{2020,3,30},{0,49,1}}, "PM10")).

test_getNotExistingValue() ->
  [
    ?_assertEqual({error, noEntries}, getOneValue("Helsinki",{{2020,2,12},{0,9,1}},"PM2.5")),
    ?_assertEqual({error, noEntries}, getOneValue("Krakow",{{2020,2,12},{0,9,1}},"temperature"))
  ].

test_removeValue() ->
  ?_assertEqual({ok, "Monitor updated"}, removeValue("Helsinki",{{2020,3,30},{0,49,1}},"PM10")).


test_getStats() ->
  [
    ?_assertEqual(123.0,  getStationMean("Helsinki", "PM10")),
    ?_assertEqual({error, noEntries}, getStationMean("Krakow",  "PM10")),
    ?_assertEqual(123.0, getDailyMean({2020,3,30}, "PM10")),
    ?_assertEqual({error, noEntries}, getDailyMean({2020,2,21}, "PM10")),
    ?_assertEqual(1, daysOverLimitInMonth("Helsinki", 2020, 3, 112, "PM10")),
    ?_assertEqual({error, noEntries}, daysOverLimitInMonth("Helsinki", 2020, 4, 112, "PM10")),
    ?_assertEqual({30,15.0}, biggestAvgInMonth(2020, 3, "temperature")),
    ?_assertEqual({error, noEntries}, biggestAvgInMonth(2020, 4, "temperature"))
  ].
