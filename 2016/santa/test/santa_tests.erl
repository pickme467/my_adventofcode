-module(santa_tests).

-include_lib("eunit/include/eunit.hrl").

% skipping long test (with skip word),
% line below prevents compilation warnings
-export([day_5a_test_skip/0, day_5b_test_skip/0]).

day_1a_test_() ->
  [ ?_assertEqual(1, santa:get_distance("R1"))
  , ?_assertEqual(2, santa:get_distance("R2"))
  , ?_assertEqual(1, santa:get_distance("L1"))
  , ?_assertEqual(2, santa:get_distance("R1, R1"))
  , ?_assertEqual(1, santa:get_distance("R1, R1, R1"))
  , ?_assertEqual(0, santa:get_distance("R1, R1, R1, R1"))
  , ?_assertEqual(1, santa:get_distance("L1, L1, L1"))
  , ?_assertEqual(0, santa:get_distance("L1, L1, L1, L1"))
  ].

day_1a_acceptance_test_() ->
  [ ?_assertEqual(5, santa:get_distance("R2, L3"))
  , ?_assertEqual(2, santa:get_distance("R2, R2, R2"))
  , ?_assertEqual(12, santa:get_distance("R5, L5, R5, R3"))
  ].

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

day_3a_test_() ->
  [ ?_assertEqual(1, santa:find_triangles(["2 3 4"]))
  , ?_assertEqual(1, santa:find_triangles(["2 3 4", "2 3 5"]))
  ].

day_3a_acceptance_test_() ->
  ?_assertEqual(1050, santa:day_3a()).

day_3b_test_() ->
  [ ?_assertEqual(1, santa:find_triangles_vertically(["2", "2", "1",
                                                      "3", "3", "2",
                                                      "4", "5", "3"]))
  ].

day_3b_acceptance_test_() ->
  ?_assertEqual(1921, santa:day_3b()).

day_4a_test_() ->
  [ ?_assertEqual("a", santa:decipher("a"))
  , ?_assertEqual("ab", santa:decipher("ab"))
  , ?_assertEqual("ba", santa:decipher("abb"))
  , ?_assertEqual("cba", santa:decipher("cccabb"))
  , ?_assertEqual("cbad", santa:decipher("cccdabb"))
  , ?_assertEqual("cbade", santa:decipher("cccdabbefg"))
  , ?_assertEqual({"a-b-c-", "123", "ala"}, santa:split_input("a-b-c-123[ala]"))
  ].

day_4a_acceptance_test_() ->
  [ ?_assertEqual("abxyz", santa:decipher("aaaaa-bbb-z-y-x-"))
  , ?_assertEqual("abcde", santa:decipher("a-b-c-d-e-f-g-h-"))
  , ?_assertEqual("oarel", santa:decipher("not-a-real-room-"))
  , ?_assertNotEqual("decoy", santa:decipher("totally-real-room-"))
  , ?_assertEqual(158835, santa:day_4a())
  ].

day_4b_test_() ->
  [ ?_assertEqual("a", santa:rotate("a", 0))
  , ?_assertEqual("b", santa:rotate("a", 1))
  , ?_assertEqual("a", santa:rotate("z", 1))
  , ?_assertEqual("aa", santa:rotate("zz", 1))
  , ?_assertEqual("very", santa:rotate("qzmt", 343))
  ].

day_4b_acceptance_test_() ->
  ?_assertEqual(993, santa:day_4b()).

day_5a_test_skip() ->
  {timeout, 300, ?_assertEqual("2414BC77", santa:day_5a())}.

day_5b_test_skip() ->
  {timeout, 500, ?_assertEqual("437E60FC", santa:day_5b())}.
