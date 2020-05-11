-module(pollution_supervisor_test).
-author("anian").

-include_lib("eunit/include/eunit.hrl").

-import(pollution_supervisor, [start_link/0]).

%% API
-export([]).

startServer() -> pollution_supervisor:start_link().

stopServer(_) -> ok.


functions_test_() ->
  {
    setup,
    local,
    fun startServer/0,
    fun stopServer/1,
    fun(_) ->
      {
        inorder,
        [
          ?_assertMatch({error, _}, pollution_gen_server:start()),
          ?_assertEqual(ok, pollution_gen_server:stop()),
          ?_assertMatch({error, _}, pollution_gen_server:start()),
          test_crash_and_rise()
        ]
      }
    end
  }.

test_crash_and_rise() ->
  pollution_gen_server:crash(),
  ?_assertEqual({ok, "Monitor updated"}, pollution_gen_server:addStation("Turku", {123.3,12.3})).