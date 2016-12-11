%%%-------------------------------------------------------------------
%% @doc display_grid top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(display_grid_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, make_grid/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

make_grid() ->
  [supervisor:start_child(?SERVER, [X, Y]) ||
    X <- lists:seq(0, 49),
    Y <- lists:seq(0, 5)].

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  {ok, { {simple_one_for_one, 0, 1},
         [#{ id => display_grid,
             start => {display_grid,
                       start_link,
                       []},
             shutdown => brutal_kill,
             type => worker,
             modules => [display_grid]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
