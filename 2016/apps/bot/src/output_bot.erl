-module(output_bot).

-behaviour(gen_server).

%% API
-export([start_link/0, output/2, get_outputs/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {outputs :: map()}).

output(Value, {output, Number}) ->
  gen_server:cast(?SERVER, {Value, {output, Number}}).

get_outputs() ->
  gen_server:call(?SERVER, get_outputs).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{outputs = #{}}}.

handle_call(get_outputs, _From, State) ->
  {reply, State#state.outputs, State};handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({Value, {output, Number}}, State) ->
  Values = maps:get(Number, State#state.outputs, []),
  NewMap = maps:put(Number, [Value | Values], State#state.outputs),
  {noreply, State#state{outputs = NewMap}};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
