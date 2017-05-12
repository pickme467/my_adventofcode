-module(santa_tests).

-include_lib("eunit/include/eunit.hrl").

                                                % skipping long test (with skip word),
                                                % line below prevents compilation warnings
-export([day_5a_test_skip/0, day_5b_test_skip/0,
         day_9b_acceptance_test_skip/0]).

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

day_6a_test_() ->
  [ ?_assertEqual(["a", "b"], santa:make_vertical("ab"))
  , ?_assertEqual(["ca", "b"], santa:make_vertical("ab\nc"))
  ].

day_6a_acceptance_test_() ->
  ?_assertEqual("kqsdmzft", santa:day_6a()).

day_6b_acceptance_test_() ->
  ?_assertEqual("tpooccyo", santa:day_6b()).

day_7a_test_() ->
  [ ?_assertNot(santa:is_tls("abcd"))
  , ?_assertNot(santa:is_tls("abcde"))
  , ?_assert(santa:is_tls("abba"))
  , ?_assertNot(santa:is_tls("aaaa"))
  , ?_assert(santa:is_tls("aabba"))
  , ?_assertEqual([{normal, "aabba"}], santa:split_tls_input("aabba"))
  , ?_assertEqual([{normal, "a"}], santa:split_tls_input("a"))
  , ?_assertEqual([{tls, "aabba"}], santa:split_tls_input("[aabba]"))
  , ?_assertEqual([{tls, "a"}, {normal, "b"}], santa:split_tls_input("[a]b"))
  , ?_assertEqual([{tls, "a"}, {normal, "b"}, {tls, "c"}],
                  santa:split_tls_input("[a]b[c]"))
  ].

day_7a_aceptance_test_() ->
  [ ?_assertEqual(1, santa:analyze_address("abba[mnop]qrst"))
  , ?_assertEqual(0, santa:analyze_address("abcd[bddb]xyyx"))
  , ?_assertEqual(0, santa:analyze_address("aaaa[qwer]tyui"))
  , ?_assertEqual(1, santa:analyze_address("ioxxoj[asdfgh]zxcvbn"))
  , ?_assertEqual(115, santa:day_7a())
  ].

day_7b_test_() ->
  [ ?_assertEqual([], santa:find_aba_bab("abc"))
  , ?_assertEqual([{aba, ["aba"]}], santa:find_aba_bab("aba"))
  , ?_assertEqual([{bab, ["aba"]}], santa:find_aba_bab("[aba]"))
  , ?_assertEqual([{bab, ["aba", "aca"]}], santa:find_aba_bab("[aba][aca]"))
  , ?_assertEqual(false, santa:is_aba_bab("[aba][aca]"))
  , ?_assertEqual(true, santa:is_aba_bab("[aba][aca]bab"))
  ].

day_7b_acceptance_test_() ->
  [ ?_assertEqual(true, santa:is_aba_bab("aba[bab]xyz"))
  , ?_assertEqual(false, santa:is_aba_bab("xyx[xyx]xyx"))
  , ?_assertEqual(true, santa:is_aba_bab("aba[bab]xyzaaa[kek]eke"))
  , ?_assertEqual(true, santa:is_aba_bab("zazbz[bzb]cdb"))
  , ?_assertEqual(231, santa:day_7b())
  ].

day_8a_test_() ->
  [ ?_assertEqual(1, santa:count_lights(["rect 1x1"]))
  , ?_assertEqual(2, santa:count_lights(["rect 2x1"]))
  , ?_assertEqual(4, santa:count_lights(["rect 2x2"]))
  , ?_assertEqual(0, santa:count_lights(["rotate column x=0 by 1"]))
  , ?_assertEqual(0, santa:count_lights(["rotate row y=0 by 1"]))
  , ?_assertEqual(1, santa:count_lights(["rotate row y=0 by 1", "rect 1x1"]))
  , ?_assertEqual(2, santa:count_lights(["rect 1x1", "rotate row y=0 by 1",
                                         "rect 1x1"]))
  , ?_assertEqual(2, santa:count_lights(["rect 1x1", "rotate column x=0 by 1",
                                         "rect 1x1"]))
  , ?_assertEqual(1, hd(hd(santa:display(["rect 1x1", "rotate column x=0 by 1",
                                          "rect 1x1"]))))
  ].

day_8a_acceptance_test_() ->
  ?_assertEqual(119, santa:day_8a()).

%% #### #### #  # ####  ### ####  ##   ##  ###   ##
%%    # #    #  # #    #    #    #  # #  # #  # #  #
%%   #  ###  #### ###  #    ###  #  # #    #  # #  #
%%  #   #    #  # #     ##  #    #  # # ## ###  #  #
%% #    #    #  # #       # #    #  # #  # #    #  #
%% #### #    #  # #    ###  #     ##   ### #     ##
day_8b_acceptance_test_() ->
  ?_assertEqual([[1,1,1,1,0,1,1,1,1,0,1,0,0,1,0,1,1,1,1,0,0,1,1,1,0,1,1,1,1,
                  0,0,1,1,0,0,0,1,1,0,0,1,1,1,0,0,0,1,1,0,0],
                 [0,0,0,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,
                  0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0,1,0,0,1,0],
                 [0,0,1,0,0,1,1,1,0,0,1,1,1,1,0,1,1,1,0,0,1,0,0,0,0,1,1,1,0,
                  0,1,0,0,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,1,0],
                 [0,1,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,0,0,0,
                  0,1,0,0,1,0,1,0,1,1,0,1,1,1,0,0,1,0,0,1,0],
                 [1,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,0,1,0,1,0,0,0,
                  0,1,0,0,1,0,1,0,0,1,0,1,0,0,0,0,1,0,0,1,0],
                 [1,1,1,1,0,1,0,0,0,0,1,0,0,1,0,1,0,0,0,0,1,1,1,0,0,1,0,0,0,
                  0,0,1,1,0,0,0,1,1,1,0,1,0,0,0,0,0,1,1,0,0]], santa:day_8b()).

day_9a_test_() ->
  [ ?_assertEqual({"U", "P"}, santa:decode_directive("(1x1)UP"))
  , ?_assertEqual("UP", santa:traverse_repeat("(1x1)UP", []))
  , ?_assertEqual("UUUP", santa:traverse_repeat("(1x3)UP", []))
  , ?_assertEqual(6, length(santa:traverse_repeat("ADVENT", [])))
  , ?_assertEqual(7, length(santa:traverse_repeat("A(1x5)BC", [])))
  , ?_assertEqual(9, length(santa:traverse_repeat("(3x3)XYZ", [])))
  , ?_assertEqual(11, length(santa:traverse_repeat("A(2x2)BCD(2x2)EFG", [])))
  , ?_assertEqual(6, length(santa:traverse_repeat("(6x1)(1x3)A", [])))
  , ?_assertEqual(18, length(santa:traverse_repeat("X(8x2)(3x3)ABCY", [])))
  , ?_assertEqual("X(3x3)ABC(3x3)ABCY",
                  santa:traverse_repeat("X(8x2)(3x3)ABCY", []))
  ].

day_9a_acceptance_test_() ->
  ?_assertEqual(97714, santa:day_9a()).

day_9b_test_() ->
  [ ?_assertEqual(length("XABCABCABCABCABCABCY"),
                  santa:traverse_repeat_intensive("X(8x2)(3x3)ABCY", [], 0))
  , {timeout, 5000, ?_assertEqual(445,
                                  santa:traverse_repeat_intensive(
                                    "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN",
                                    [], 0))}
  , ?_assertEqual(241920,
                  santa:traverse_repeat_intensive(
                    "(27x12)(20x12)(13x14)(7x10)(1x12)A",
                    [], 0))
  ].

day_9b_acceptance_test_skip() ->
  {timeout, 10000, ?_assertEqual(10762972461, santa:day_9b())}.

day_10a_test_() ->
  [ ?_assertEqual({value, 10, 15}, santa:decode_bot("value 10 goes to bot 15"))
  , ?_assertEqual(
       {{bot, 35}, {bot, 24}, {bot, 187}},
       santa:decode_bot("bot 35 gives low to bot 24 and high to bot 187"))
  , ?_assertEqual(
       {{bot, 35}, {output, 0}, {bot, 187}},
       santa:decode_bot("bot 35 gives low to output 0 and high to bot 187"))
  , ?_assertMatch(
       {[{{bot, 35}, {output, 0}, {bot, 187}}], _},
       santa:build_machine("bot 35 gives low to output 0 and high to bot 187"))
  , ?_assertMatch(
       {_, [{10, {bot, 15}}]},
       santa:build_machine("value 10 goes to bot 15"))
  , ?_assertEqual(
       {[{{bot, 35}, {output, 0}, {bot, 187}}], [{10, {bot, 15}}]},
       santa:build_machine("bot 35 gives low to output 0 and high to bot 187\n"
                           "value 10 goes to bot 15"))
  ].

day_10a_acceptance_test_() ->
  {timeout, 10000, ?_assertEqual(118, santa:day_10a())}.

day_10b_acceptance_test_() ->
  {timeout, 20000, ?_assertEqual(143153, santa:day_10b())}.

day_11a_test_() ->
  [ ?_assert(santa:check_floor([]))
  , ?_assert(santa:check_floor(["pog"]))
  , ?_assert(santa:check_floor(["pom"]))
  , ?_assert(santa:check_floor(["pog", "pom"]))
  , ?_assert(santa:check_floor(["pog", "prg"]))
  , ?_assert(santa:check_floor(["rum", "pom"]))
  , ?_assert(santa:check_floor(["rug", "pog", "pom"]))
  , ?_assert(santa:check_floor(["rug", "pog", "pom", "rum"]))
  , ?_assert(santa:check_floor(["rug", "pog", "pom", "cog"]))
  , ?_assert(santa:check_floor(["rug", "pog", "cog"]))
  , ?_assert(santa:check_floor(["rum", "pom", "com"]))
  , ?_assertNot(santa:check_floor(["rug", "pog", "pom", "com"]))
  , ?_assert(santa:is_valid(santa:day_11a_input()))
  , ?_assertEqual([], santa:generate_elevator_content([]))
  , ?_assertEqual([["pom"]], santa:generate_elevator_content(["pom"]))
  , ?_assertEqual(lists:sort([["pom"], ["pog"], ["pom", "pog"]]),
                  lists:sort(santa:generate_elevator_content(["pom", "pog"])))
  , ?_assertEqual(
       lists:sort([["pom"], ["pog"], ["cog"], ["pom", "pog"],
                   ["pom", "cog"], ["pog", "cog"]]),
       lists:sort(santa:generate_elevator_content(["cog", "pom", "pog"])))
  , ?_assertEqual(lists:sort([{1, []}, {4, []}]),
                  lists:sort(santa:get_other_floors([2, 3],
                                                    [{4, []}, {3, []},
                                                     {1, []}, {2, []}])))
  , ?_assert(lists:all(fun (Setup) ->
                           santa:is_valid(Setup)
                       end, santa:make_floors(santa:day_11a_input())))
  , ?_assertEqual(7, length(santa:make_floors(santa:day_11a_input())))
  , ?_assertEqual([{{elevator,3},
                    [{1,[a,b]},
                     {2,[c,d]},
                     {3,[floor,floor]},
                     {4,[ele,ele]}]}],
                  santa:swap_floors([3, 4],
                                    [{1, [a, b]},
                                     {2, [c, d]},
                                     {3, []},
                                     {4, []}], [{[floor, floor], [ele, ele]}]))
  , ?_assertEqual([{["pom"], ["pog"]},{["pog"], ["pom"]}],
                  santa:make_new_floor_elevator_floor_set(
                    [], ["pom", "pog"],
                    [["pom"], ["pog"]]))
  , ?_assertEqual(3,
                  santa:find_best_route({{elevator, 1},
                                         [{1, ["pom", "pog"]},
                                          {2, []},
                                          {3, []},
                                          {4, []}]}))
  , ?_assertEqual(11,
                  santa:find_best_route({{elevator, 1},
                                         [{1, ["ham", "lam"]},
                                          {2, ["hag"]},
                                          {3, ["lag"]},
                                          {4, []}]}))
  , ?_assertMatch({{elevator, 1}, [{1,1}, {1,1}]},
                  santa:normalize({{elevator, 1}, [{1, ["pom", "eom", "pog", "eog"]},
                                                   {2, []},
                                                   {3, []},
                                                   {4, []}]}))
  , ?_assertEqual({not_important, [{1,1}, {2, 3}]},
                  santa:normalize([], not_important,
                                  #{b => #{"m" => 2, "g" => 3},
                                    a => #{"m" => 1, "g" => 1}}))
  , ?_assertEqual({not_important, [{1,1}, {2, 3}]},
                  santa:normalize([{1, ["pom", "pog"]}, {2, ["eam"]}, {3, ["eag"]}],
                                  not_important, #{}))
  ].

day_11a_acceptance_test_() ->
  {timeout, 2000, ?_assertEqual(47, santa:day_11a())}.

day_11b_acceptance_test_() ->
  {timeout, 20000, ?_assertEqual(71, santa:day_11b())}.
