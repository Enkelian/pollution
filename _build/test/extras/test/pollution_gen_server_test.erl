-module(pollution_gen_server_test).
-author("anian").

-include_lib("eunit/include/eunit.hrl").
-import(pollution_gen_server, [start/0, init/0, stop/0, getMonitor/0, addValue/4, addStation/2, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, daysOverLimitInMonth/5, biggestAvgInMonth/3]).


stopServer(_) ->
  stop().

startServer() ->
  start(),
  addStation("Turku", {123.3,12.3}),
  addStation("Helsinki", {333,222}),
  addValue("Helsinki", {{2020,3,30},{0,49,1}}, "PM10", 111),
  addValue({333,222}, {{2020,3,30},{0,8,1}}, "PM10", 123),
  addValue({333,222}, {{2020,3,30},{0,9,1}}, "PM10", 123),
  addValue("Turku", {{2020,3,30},{0,9,1}}, "PM10", 123),
  addValue("Turku", {{2020,3,30},{0,9,1}}, "temperature", 15),
  addValue("Helsinki", {{2020,3,30},{0,9,1}}, "temperature", 12),
  addValue("Helsinki", {{2020,2,12},{0,9,1}}, "temperature", 13).


adding_test_() ->
  {
    "Tries adding existing and invalid stations and values.",
    {
      setup,
      fun startServer/0,
      fun stopServer/1,
      fun () ->
        {
          inorder,
          [
            test_addExistingStations(),
            test_addInvalidStations(),
            test_addInvalidValue(),
            test_addExistingValue()
          ]
        }
      end
    }
  }.

getRemove_test_() ->
  { "Gets existing value, tries to get non-existing value, removes a value and then tries to get it.",
    {
      setup,
      fun startServer/0,
      fun stopServer/1,
      fun () ->
        {
          inorder,
          [
            test_getExistingValue(),
            test_getNotExistingValue(),
            test_removeValue()
          ]
        }
      end
    }
  }.

stats_test_() ->
  { "Tests functions returning certain statistics.",
    {
      setup,
      fun startServer/0,
      fun stopServer/1,
      fun () ->
        {
          inorder,
          [
            test_getStationMean(),
            test_getDailyMean(),
            test_daysOverLimitInMonth(),
            test_biggestAvgInMonth()
          ]
        }
      end
    }
  }.

test_addExistingStations() ->
  [
    ?_assertEqual({error, invalidEntry}, addStation("Turku", {123.3,12.3})),
    ?_assertEqual({error, invalidEntry}, addStation("Helsinki", {123.3,12.3}))
  ].


test_addInvalidStations() ->
  [
    ?_assertEqual({error, invalidEntry}, addStation(123, {13.3,12.3})),
    ?_assertEqual({error, invalidEntry}, addStation("Helsinki", 1))
  ].

test_addInvalidValue() ->
  [
    ?_assertEqual({error, invalidData}, addValue("Helsinki", {{2020,2,30},{0,49,1}}, "PM10", 111)),
    ?_assertEqual({error, invalidData}, addValue({3,222}, {{2020,3,30},{0,49,1}}, "PM10", 111))
  ].

test_addExistingValue() ->
  ?_assertEqual({error, alreadyExists}, addValue({333,222}, {{2020,3,30},{0,49,1}}, "PM10", 111)).

test_getExistingValue() ->
  [
    ?_assertEqual(13, getOneValue("Helsinki",{{2020,2,12},{0,9,1}},"temperature")),
    ?_assertEqual(111, getOneValue("Helsinki",{{2020,3,30},{0,49,1}},"PM10"))
  ].

test_getNotExistingValue() ->
  [
    ?_assertEqual({error, noEntries}, getOneValue("Helsinki",{{2020,2,12},{0,9,1}},"PM2.5")),
    ?_assertEqual({error, noEntries}, getOneValue("Krakow",{{2020,2,12},{0,9,1}},"temperature")),
    ?_assertEqual({error, noEntries}, getOneValue("Helsinki",{{2020,2,12},{0,9,2}},"PM10"))
  ].

test_removeValue() ->
  MRem = removeValue("Helsinki",{{2020,3,30},{0,49,1}},"PM10"),
  ?_assertEqual({error, noEntries}, getOneValue("Helsinki",{{2020,3,30},{0,49,1}},"PM10")).

test_getStationMean() ->
  [
    ?_assertEqual(119.0,  getStationMean("Helsinki", "PM10")),
    ?_assertEqual(120.0, getDailyMean({2020,3,30}, "PM10")),
    ?_assertEqual({error, noEntries}, getStationMean("Krakow",  "PM10"))
  ].

test_getDailyMean() ->
  [
    ?_assertEqual(120.0, getDailyMean({2020,3,30}, "PM10")),
    ?_assertEqual({error, noEntries}, getDailyMean({2020,2,21}, "PM10"))
  ].

test_daysOverLimitInMonth() ->
  [
    ?_assertEqual(2, daysOverLimitInMonth("Helsinki", 2020, 3, 112, "PM10")),
    ?_assertEqual({error, noEntries}, daysOverLimitInMonth("Helsinki", 2020, 4, 112, "PM10"))
  ].

test_biggestAvgInMonth() ->
  [
    ?_assertEqual({30,13.5}, biggestAvgInMonth(2020, 3, "temperature")),
    ?_assertEqual({error, noEntries}, biggestAvgInMonth(2020, 4, "temperature"))
  ].
