-module(pollution_supervisor).
-behavior(supervisor).

%% API
-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 1000,
  MaxSecondsBetweenRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  AChild = {'pollution_gen_server', {'pollution_gen_server', start, []},
    Restart, Shutdown, Type, ['pollution_gen_server']},

  {ok, {SupFlags, [AChild]}}.

