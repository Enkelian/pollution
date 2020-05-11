-module(pollutiontest).

-include_lib("eunit/include/eunit.hrl").
-import(pollution, [createMonitor/0, addStation/2, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, daysOverLimitInMonth/6, biggestAvgInMonth/4]).

start() ->
  M = createMonitor(),
  M1 = addStation(M, {"Turku", {123.3,12.3}}),
  M2 = addStation(M1, {"Helsinki", {333,222}}),
  M3 = addValue(M2, "Helsinki", {{2020,3,30},{0,49,1}}, "PM10", 111),
  M4 = addValue(M3, {333,222}, {{2020,3,30},{0,8,1}}, "PM10", 123),
  M5 = addValue(M4, {333,222}, {{2020,3,30},{0,9,1}}, "PM10", 123),
  M6 = addValue(M5, "Turku", {{2020,3,30},{0,9,1}}, "PM10", 123),
  M7 = addValue(M6, "Turku", {{2020,3,30},{0,9,1}}, "temperature", 15),
  M8 = addValue(M7, "Helsinki", {{2020,3,30},{0,9,1}}, "temperature", 12),
  M9 = addValue(M8, "Helsinki", {{2020,2,12},{0,9,1}}, "temperature", 13).

stop(M) ->
  ok.

adding_test_() ->
  {
    "Tries adding existing and invalid stations and values.",
    {
      setup,
      fun start/0,
      fun stop/1,
      fun (M) ->
        {
          inparallel,
          [
            test_addExistingStations(M),
            test_addInvalidStations(M),
            test_addInvalidValue(M),
            test_addExistingValue(M)
          ]
        }
      end
    }
  }.

getRemove_test_() ->
  { "Gets existing value, tries to get non-existing value, removes a value and then tries to get it.",
    {
      setup,
      fun start/0,
      fun stop/1,
      fun (M) ->
        {
          inparallel,
          [
%%            test_getExistingValue(M),
            test_getNotExistingValue(M),
            test_removeValue(M)
          ]
        }
      end
    }
  }.

stats_test_() ->
  { "Tests functions returning certain statistics.",
    {
      setup,
      fun start/0,
      fun stop/1,
      fun (M) ->
        {
          inparallel,
          [
            test_getStationMean(M),
            test_getDailyMean(M),
            test_daysOverLimitInMonth(M),
            test_biggestAvgInMonth(M)
          ]
        }
      end
    }
  }.

test_addExistingStations(M) ->
  [
    ?_assertEqual({error, invalidEntry}, addStation(M, {"Turku", {123.3,12.3}})),
    ?_assertEqual({error, invalidEntry}, addStation(M, {"Helsinki", {123.3,12.3}}))
  ].


test_addInvalidStations(M) ->
  [
    ?_assertEqual({error, invalidEntry}, addStation(M, {123, {13.3,12.3}})),
    ?_assertEqual({error, invalidEntry}, addStation(M, {"Helsinki", 1})),
    ?_assertEqual({error,notAMonitor}, addStation("Not a monitor", {"Krakow", {125,163}}))
  ].

test_addInvalidValue(M) ->
  [
    ?_assertEqual({error, invalidData}, addValue(M, "Helsinki", {{2020,2,30},{0,49,1}}, "PM10", 111)),
    ?_assertEqual({error, invalidData}, addValue(M, {3,222}, {{2020,3,30},{0,49,1}}, "PM10", 111))
  ].

test_addExistingValue(M) ->
    ?_assertEqual({error, alreadyExists}, addValue(M, {333,222}, {{2020,3,30},{0,49,1}}, "PM10", 111)).

test_getExistingValue(M) ->
  [
    ?_assertEqual(13, getOneValue(M, "Helsinki",{{2020,2,12},{0,9,1}},"temperature")),
    ?_assertEqual(111, getOneValue(M, "Helsinki",{{2020,3,30},{0,49,1}},"PM10"))
  ].

test_getNotExistingValue(M) ->
  [
    ?_assertEqual({error, noEntries}, getOneValue(M, "Helsinki",{{2020,2,12},{0,9,1}},"PM2.5")),
    ?_assertEqual({error, noEntries}, getOneValue(M, "Krakow",{{2020,2,12},{0,9,1}},"temperature")),
    ?_assertEqual({error, noEntries}, getOneValue(M, "Helsinki",{{2020,2,12},{0,9,2}},"PM10"))
  ].

test_removeValue(M) ->
  MRem = removeValue(M,"Helsinki",{{2020,3,30},{0,49,1}},"PM10"),
  ?_assertEqual({error, noEntries}, getOneValue(MRem, "Helsinki",{{2020,3,30},{0,49,1}},"PM10")).

test_getStationMean(M) ->
  [
    ?_assertEqual(119.0,  getStationMean(M, "Helsinki", "PM10")),
    ?_assertEqual(120.0, getDailyMean(M, {2020,3,30}, "PM10")),
    ?_assertEqual({error, noEntries}, getStationMean(M, "Krakow",  "PM10"))
  ].

test_getDailyMean(M) ->
  [
    ?_assertEqual(120.0, getDailyMean(M, {2020,3,30}, "PM10")),
    ?_assertEqual({error, noEntries}, getDailyMean(M, {2020,2,21}, "PM10"))
  ].

test_daysOverLimitInMonth(M) ->
  [
    ?_assertEqual(2, daysOverLimitInMonth(M, "Helsinki", 2020, 3, 112, "PM10")),
    ?_assertEqual({error, noEntries}, daysOverLimitInMonth(M, "Helsinki", 2020, 4, 112, "PM10"))
  ].

test_biggestAvgInMonth(M) ->
  [
    ?_assertEqual({30,13.5}, biggestAvgInMonth(M, 2020, 3, "temperature")),
    ?_assertEqual({error, noEntries}, biggestAvgInMonth(M, 2020, 4, "temperature"))
  ].


