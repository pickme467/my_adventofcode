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

day_2a_test_() ->
  [ ?_assertEqual(4, santa:next_key(5, "L"))
  , ?_assertEqual(5, santa:next_key(6, "L"))
  , ?_assertEqual(2, santa:next_key(3, "L"))
  , ?_assertEqual(1, santa:next_key(2, "L"))
  , ?_assertEqual(1, santa:next_key(1, "L"))
  , ?_assertEqual(1, santa:next_key(4, "U"))
  , ?_assertEqual(2, santa:next_key(5, "U"))
  , ?_assertEqual(3, santa:next_key(3, "U"))
  , ?_assertEqual(6, santa:next_key(3, "D"))
  , ?_assertEqual(3, santa:next_key(2, "R"))
  , ?_assertEqual([5], santa:decipher_code(5, ["LR"]))
  , ?_assertEqual([2], santa:decipher_code(2, ["LR"]))
  , ?_assertEqual([3], santa:decipher_code(2, ["RRR"]))
  , ?_assertEqual([3, 6], santa:decipher_code(2, ["RRR", "D"]))
  , ?_assertEqual(5, santa:next_key_weird_keyboard(5, "L"))
  , ?_assertEqual($A, santa:next_key_weird_keyboard($B, "L"))
  , ?_assertEqual($A, santa:next_key_weird_keyboard($B, "L"))
  , ?_assertEqual(6, santa:next_key_weird_keyboard(5, "R"))
  , ?_assertEqual(5, santa:next_key_weird_keyboard(5, "U"))
  , ?_assertEqual($B, santa:next_key_weird_keyboard($D, "U"))
  , ?_assertEqual($D, santa:next_key_weird_keyboard($B, "D"))
  ].

day_2a_acceptance_test_() ->
  [ ?_assertEqual([1, 9, 8, 5], santa:decipher_code(5, ["ULL",
                                                        "RRDDD",
                                                        "LURDL",
                                                        "UUUUD"]))
  , ?_assertEqual([1,8,8,4,3], santa:day_2a())
  ].

day_2b_acceptance_test_() ->
  [ ?_assertEqual([5, $D, $B, 3],
                  santa:decipher_code_weird(5, ["ULL",
                                                "RRDDD",
                                                "LURDL",
                                                "UUUUD"]))
  , ?_assertEqual([6, 7, $B, $B, 9], santa:day_2b())
  ].
