-module(bot).

-behaviour(gen_server).

%% API
-export([start_link/1, make_bot_name/1, hand_piece/2, history/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {my_number :: integer(),
                low_name :: {atom, integer()},
                high_name :: {atom, integer()},
                received :: list(),
                history :: list()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link({Bot, Lower, Higher}) ->
  BotName = make_bot_name(Bot),
  gen_server:start_link({local, BotName}, ?MODULE, [Bot, Lower, Higher], []).

make_bot_name({Name, Number}) ->
    list_to_atom(atom_to_list(Name) ++ integer_to_list(Number, 10)).

hand_piece(Value, {output, Number}) ->
  output_bot:output(Value, {output, Number}),
  ok;
hand_piece(Value, {bot, Number}) ->
  hand_piece(Value, make_bot_name({bot, Number}));
hand_piece(Value, BotName) ->
  gen_server:cast(BotName, {value, Value}).

history(Bot) when is_pid(Bot) ->
  gen_server:call(Bot, history);
history(Bot) ->
  gen_server:call(make_bot_name(Bot), history).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{_Bot, MyNumber}, LowName, HighName]) ->
  process_flag(trap_exit, true),
  {ok, #state{my_number = MyNumber, low_name = LowName, high_name = HighName,
              received = [], history = []}}.

handle_call(history, _From, State) ->
  {reply, {State#state.my_number, State#state.history}, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({value, Value}, #state{received = Received} = State)
  when length(Received) =:= 0 ->
  {noreply, State#state{received = [Value]}};
handle_cast({value, Value}, #state{received = Received} = State)
  when length(Received) =:= 1 ->
  [Low, High] = lists:sort([Value | Received]),
  hand_piece(Low, State#state.low_name),
  hand_piece(High, State#state.high_name),
   {noreply, State#state{received = [],
                        history = [{Low, High} | State#state.history]}};
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
