%%%-------------------------------------------------------------------
%% @doc bot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bot_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, make_bot/1, get_bots/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  output_bot:start_link(),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

make_bot(Specs) ->
  supervisor:start_child(?MODULE, [Specs]).

stop() ->
  exit(whereis(output_bot), normal),
  exit(whereis(?MODULE), normal).

get_bots() ->
  lists:map(fun({_, Pid, _, _}) ->
                Pid
            end, supervisor:which_children(?MODULE)).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  Bot = #{id => bot,
          start => {bot, start_link, []},
          shutdown => brutal_kill,
          type => worker,
          modules => [bot]},
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 0,
               period => 1},
  {ok, {SupFlags, [Bot]}}.

%%====================================================================
%% Internal functions
%%====================================================================
