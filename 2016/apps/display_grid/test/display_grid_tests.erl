-module(display_grid_tests).

-include_lib("eunit/include/eunit.hrl").

display_grid_test_() ->
  [ ?_assertEqual(display_grid_1_1, display_grid:get_name(1, 1))
  , ?_assertEqual(display_grid_0_0, display_grid:get_name(0, 0))
  , ?_assertEqual(display_grid_1_0, display_grid:get_name(1, 6))
  , ?_assertEqual(display_grid_0_0, display_grid:get_name(50, 12))
  ].

display_grid_process_test_() ->
  {setup,
   fun() ->
       {ok, Sup} = display_grid_sup:start_link(),
       {ok, Pid} = supervisor:start_child(Sup, [1, 2]),
       {Sup, Pid}
   end,
   fun({Sup, Pid}) ->
       supervisor:terminate_child(Sup, Pid),
       terminate_supervisor(Sup)
   end,
   fun({_Sup, Pid}) ->
       ?_assertEqual(Pid, erlang:whereis(display_grid_1_2))
   end}.

display_grid_all_grid_test_() ->
  {setup,
   fun() ->
       {ok, Sup} = display_grid_sup:start_link(),
       display_grid_sup:make_grid(),
       Sup
   end,
   fun(Sup) ->
       terminate_supervisor(Sup)
   end,
   fun(_Sup) ->
       [?_assertEqual(true, (erlang:is_process_alive(
                               erlang:whereis(display_grid_0_0))))
       ,?_assertEqual(true, (erlang:is_process_alive(
                               erlang:whereis(display_grid_49_5))))
       ]
   end}.


terminate_supervisor(Sup) ->
  Ref = monitor(process, Sup),
  exit(Sup, normal),
  receive
    {'DOWN', Ref, process, Sup, _Reason} ->
      ok
  after 1000 ->
      error(exit_timeout)
  end.
