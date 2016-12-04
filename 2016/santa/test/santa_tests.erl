-module(santa_tests).

-include_lib("eunit/include/eunit.hrl").

% skipped test warning suppression
%-export([day_1b_acceptance_test_skip/0]).

day_1b_test_() ->
  [ ?_assertEqual([{1, 0}], santa:walk({north, {0, 0}}, "R1"))
  , ?_assertEqual([{2, 0}], santa:walk({north, {1, 0}}, "R1"))
  , ?_assertEqual([{0, 0}], santa:walk({north, {1, 0}}, "L1"))
  , ?_assertEqual([{0, 0}, {-1, 0}],
                  santa:walk({north, {1, 0}}, "L2"))
  , ?_assertEqual([{2, 0}, {3, 0}],
                  santa:walk({north, {1, 0}}, "R2"))
  , ?_assertEqual([{2, 5}, {3, 5}],
                  santa:walk({north, {1, 5}}, "R2"))
  , ?_assertEqual([{0, 7}, {-1, 7}],
                  santa:walk({north, {1, 7}}, "L2"))
  , ?_assertEqual([{0, 0}], santa:walk({south, {1, 0}}, "R1"))
  , ?_assertEqual([{2, 0}], santa:walk({south, {1, 0}}, "L1"))
  , ?_assertEqual([{1, 1}], santa:walk({east, {1, 0}}, "L1"))
  , ?_assertEqual([{1, 1}, {1, 2}], santa:walk({east, {1, 0}}, "L2"))
  , ?_assertEqual([{1, 1}, {1, 2}], santa:walk({west, {1, 0}}, "R2"))
  , ?_assertEqual([{1, 1}, {1, 0}], santa:walk({west, {1, 2}}, "L2"))
  , ?_assertEqual([{1, 1}, {1, 0}], santa:walk({east, {1, 2}}, "R2"))
  , ?_assertEqual(1, santa:calculate_distance("R1"))
  , ?_assertEqual(2, santa:calculate_distance("R2"))
  , ?_assertEqual(2, santa:calculate_distance("L2"))
  , ?_assertEqual(2, santa:calculate_distance("R1, R1"))
  , ?_assertEqual(1, santa:calculate_distance("R2, R1, R1, R2"))
  ].

day_1b_acceptance_test_() ->
  [ ?_assertEqual(4, santa:calculate_distance("R8, R4, R4, R8"))
  , ?_assertEqual(153, santa:day_1b())
  ].
