%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2016,
%%% @doc
%%%
%%% @end
%%% Created : 10 Dec 2016 by  <>
%%%-------------------------------------------------------------------
-module(display_grid).

-behaviour(gen_fsm).

-include_lib("eunit/include/eunit.hrl").
%% API
-export([start_link/1, start_link/2, get_name/2, execute/1, count_lights/0,
        display/0]).

%% gen_fsm callbacks
-export([init/1, ready/2, ready/3, waiting_for_rows/2,
         waiting_for_my_row/2, waiting_for_columns/2,
         waiting_rotate_finish/2, waiting_row_column_update/2,
         waiting_my_new_value/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

-record(state, {me_x, me_y, next, down, lit, requestor, new_lit}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Name) ->
  gen_fsm:start_link({local, Name}, ?MODULE, [], []).

start_link(X, Y) ->
  gen_fsm:start_link({local, get_name(X, Y)}, ?MODULE,
                     [X, Y], []).

get_name(X, Y) when X > 49 ->
  get_name(X - 49 - 1, Y);
get_name(X, Y) when Y > 5 ->
  get_name(X, Y - 5 - 1);
get_name(X, Y) ->
  list_to_atom(atom_to_list(?SERVER) ++ "_" ++ integer_to_list(X)
               ++ "_" ++ integer_to_list(Y)).

execute({rectangle, [_, _]} = Request) ->
  gen_fsm:sync_send_event(get_name(0, 0), Request);
execute({rotate_row, [Row, _]} = Request) ->
  gen_fsm:sync_send_event(get_name(0, Row), Request);
execute({rotate_column, [Column, _]} = Request) ->
  gen_fsm:sync_send_event(get_name(Column, 0), Request).

count_lights() ->
  gen_fsm:sync_send_event(get_name(0, 0), count_lights).

display() ->
  [ gen_fsm:sync_send_event(get_name(0, 0), display)
  , gen_fsm:sync_send_event(get_name(0, 1), display)
  , gen_fsm:sync_send_event(get_name(0, 2), display)
  , gen_fsm:sync_send_event(get_name(0, 3), display)
  , gen_fsm:sync_send_event(get_name(0, 4), display)
  , gen_fsm:sync_send_event(get_name(0, 5), display)
  ].

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

init([]) ->
  process_flag(trap_exit, true),
  {ok, ready, #state{}};
init([X, Y]) ->
  process_flag(trap_exit, true),
  {ok, ready, #state{me_x = X, me_y = Y,
                     next = get_name(X + 1, Y),
                     down = get_name(X, Y + 1),
                     lit = 0}}.
ready({display, List}, State) ->
  request_display(State, List),
  {next_state, ready, State};
ready({rotate_column_next, [X, A]}, State) ->
  send_my_value(State, X, State#state.me_y + A),
  rotate_column_next(State, X, A),
  {next_state, waiting_my_new_value, State};
ready({rotate_row_next, [Y, A]}, State) ->
  send_my_value(State, State#state.me_x + A, Y),
  rotate_row_next(State, Y, A),
  {next_state, waiting_my_new_value, State};
ready({new_value, Lit}, State) ->
  {next_state, waiting_row_column_update, State#state{new_lit = Lit}};
ready({count_column, Sum}, State) ->
  count_row(State, Sum + State#state.lit),
  {next_state, waiting_for_my_row, State};
ready({count_row, Sum}, State) ->
  count_row(State, Sum + State#state.lit),
  {next_state, ready, State};
ready(_Event, State) ->
  {next_state, ready, State}.

ready(display, From, State) ->
  request_display(State, []),
  {next_state, waiting_for_rows, State#state{requestor = From}};
ready({rotate_column, [X, A]}, From, State) ->
  send_my_value(State, X, State#state.me_y + A),
  rotate_column_next(State, X, A),
  {next_state, waiting_rotate_finish, State#state{requestor = From}};
ready({rotate_row, [Y, A]}, From, State) ->
  send_my_value(State, State#state.me_x + A, Y),
  rotate_row_next(State, Y, A),
  {next_state, waiting_rotate_finish, State#state{requestor = From}};
ready({rectangle, [X, Y]}, _From, State) ->
  send_rectangle_next(State, X - 1 , Y),
  send_rectangle_down(State, X, Y - 1),
  {reply, ok, ready, State#state{lit = 1}};
ready(count_lights, From, State) ->
  count_row(State, State#state.lit),
  {next_state, waiting_for_rows, State#state{requestor = From}};
ready(_Event, _From, State) ->
  Reply = 1,
  {reply, Reply, ready, State}.

waiting_for_rows({display, List}, State) ->
  gen_fsm:reply(State#state.requestor, lists:reverse(List)),
  {next_state, ready, State};
waiting_for_rows({count_row, Sum}, State) ->
  count_columns(State, Sum),
  {next_state, waiting_for_columns, State}.

waiting_for_my_row({count_row, Sum}, State) ->
  count_columns(State, Sum),
  {next_state, ready, State}.

waiting_for_columns({count_column, Sum}, State) ->
  gen_fsm:reply(State#state.requestor, Sum),
  {next_state, ready, State}.

waiting_rotate_finish({new_value, Lit}, State) ->
  {next_state, waiting_rotate_finish, State#state{lit = Lit}};
waiting_rotate_finish({rotate_row_next, [_, _]}, State) ->
  gen_fsm:reply(State#state.requestor, ok),
  {next_state, ready, State};
waiting_rotate_finish({rotate_column_next, [_, _]}, State) ->
  gen_fsm:reply(State#state.requestor, ok),
  {next_state, ready, State}.

waiting_row_column_update({rotate_column_next, [X, A]}, State) ->
  send_my_value(State, X, State#state.me_y + A),
  rotate_column_next(State, X, A),
  {next_state, ready, State#state{lit = State#state.new_lit}};
waiting_row_column_update({rotate_row_next, [Y, A]}, State) ->
  send_my_value(State, State#state.me_x + A, Y),
  rotate_row_next(State, Y, A),
  {next_state, ready, State#state{lit = State#state.new_lit}}.

waiting_my_new_value({new_value, Lit}, State) ->
  {next_state, ready, State#state{lit = Lit}}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

count_columns(State, Sum) ->
  gen_fsm:send_event(State#state.next, {count_column, Sum}).

count_row(State, Sum) ->
  gen_fsm:send_event(State#state.down, {count_row, Sum}).

send_rectangle_next(_State, 0, _) ->
  ok;
send_rectangle_next(State, X, Y) ->
  gen_fsm:sync_send_event(State#state.next, {rectangle, [X, Y]}).

send_rectangle_down(_State, _, 0) ->
  ok;
send_rectangle_down(State, X, Y) ->
  gen_fsm:sync_send_event(State#state.down, {rectangle, [X, Y]}).

send_my_value(State, X, Y) ->
  gen_fsm:send_event(get_name(X, Y),
                     {new_value, State#state.lit}).

rotate_row_next(State, Y, A) ->
  gen_fsm:send_event(State#state.next, {rotate_row_next, [Y, A]}).

rotate_column_next(State, X, A) ->
  gen_fsm:send_event(State#state.down, {rotate_column_next, [X, A]}).

request_display(State, List) ->
  gen_fsm:send_event(State#state.next, {display, [State#state.lit] ++ List}).
