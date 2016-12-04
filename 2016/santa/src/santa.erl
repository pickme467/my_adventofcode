-module(santa).

-export([day_1b/0, day_2a/0, day_2b/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

day_1b() ->
  calculate_distance(day_1a_input()).

calculate_distance(List) ->
  {{_, {A, B}}, _} = apply_directions(List),
  abs(A) + abs(B).

apply_directions(List) ->
  lists:foldl(fun traverse/2,
              {{north, {0, 0}}, [{0, 0}]},
              string:tokens(List, ", ")).

traverse(_Step, {Start, found_crossing}) ->
  {Start, found_crossing};
traverse(Step, {Start, History}) ->
  Steps = walk(Start, Step),
  find_new_point_or_crossing(Step, Start, History, Steps).

find_new_point_or_crossing(Step, Start, History, Steps) ->
  case find_crossing(Steps, History) of
    {true, Point} ->
      {{new_direction(Step, Start), Point}, found_crossing};
    false ->
      {{new_direction(Step, Start),
        final_point(Steps)}, History ++ Steps}
  end.

find_crossing(Steps, History) ->
  find_crossing(false, Steps, History).

find_crossing(_, [], _) ->
  false;
find_crossing(false, [P | T], History) ->
  case lists:member(P, History) of
    true ->
      {true, P};
    false -> find_crossing(false, T, History)
  end.

new_direction([Dir | _], {Start, _}) ->
  get_direction(Dir, Start).

get_direction($R, north) -> east;
get_direction($R, east)  -> south;
get_direction($R, south) -> west;
get_direction($R, west)  -> north;
get_direction($L, north) -> west;
get_direction($L, west)  -> south;
get_direction($L, south) -> east;
get_direction(_, _)      -> north.

final_point(List) ->
  [Point | _] = lists:reverse(List),
  Point.

walk({Direction, {X, Y}}, [Turn | Steps]) ->
  StepsToGo = list_to_integer(Steps),
  walk({Direction, {X, Y}}, Turn, StepsToGo).

walk({Direction, {X, Y}}, Turn, Steps) when
    (Direction =:= north andalso Turn =:= $R) orelse
    (Direction =:= south andalso Turn =:= $L) ->
  [{Step, Y} || Step <- go_up(X, Steps)];
walk({Direction, {X, Y}}, Turn, Steps) when
    (Direction =:= west andalso Turn =:= $L);
    (Direction =:= east andalso Turn =:= $R) ->
  [{X, Step} || Step <- go_down(Y, Steps)];
walk({Direction, {X, Y}}, _, Steps) when
    Direction =:= east; Direction =:= west ->
  [{X, Step} || Step <- go_up(Y, Steps)];
walk({_, {X, Y}}, _, Steps) ->
  [{Step, Y} || Step <- go_down(X, Steps)].

go_down(Start, StepsToGo) ->
  lists:seq(Start - 1, Start - StepsToGo, -1).

go_up(Start, StepsToGo) ->
  lists:seq(Start + 1, Start + StepsToGo).

day_2a() ->
  decipher_code(5, string:tokens(day_2a_input(), "\n")).

decipher_code(Key, Recipe) ->
  decipher_code(Key, Recipe, fun next_key/2).

decipher_code(Key, Recipe, KeypadFunction) ->
  decipher_code(Key, Recipe, [], KeypadFunction).

decipher_code(_, [], Result, _) ->
  lists:reverse(Result);
decipher_code(Start, [Lines | Rest], Result, KeypadFunction) ->
  FoundKey = lists:foldl(fun (Direction, Key) ->
                  KeypadFunction(Key, [Direction])
              end, Start, Lines),
  decipher_code(FoundKey, Rest, [FoundKey] ++ Result, KeypadFunction).

next_key(X, "L") ->
  match_key_with_allowed(X, [3, 2, 6, 5, 9, 8], -1);
next_key(X, "R") ->
  match_key_with_allowed(X, [1, 2, 4, 5, 7, 8], 1);
next_key(X, "U") ->
  match_key_with_allowed(X, [4, 5, 6, 7, 8, 9], -3);
next_key(X, "D") ->
  match_key_with_allowed(X, [1, 2, 3, 4, 5, 6], 3).

match_key_with_allowed(X, Allowed, ValueToAdd) ->
  case lists:member(X, Allowed) of
    true -> X + ValueToAdd;
    false -> X
  end.

day_2b() ->
  decipher_code_weird(5, string:tokens(day_2a_input(), "\n")).

decipher_code_weird(Start, Recipe) ->
  decipher_code(Start, Recipe, fun next_key_weird_keyboard/2).

next_key_weird_keyboard(X, "L") ->
  match_key_with_allowed(X, [3, 4, 6, 7, 8, 9, $B, $C], -1);
next_key_weird_keyboard(X, "R") ->
  match_key_with_allowed(X, [2, 3, 5, 6, 7, 8, $A, $B], 1);
next_key_weird_keyboard(X, "U") ->
  MovesUp = #{
    $A => 6,
    6  => 2,
    $D => $B,
    $B => 7,
    7  => 3,
    3  => 1,
    $C => 8,
    8  => 4},
  maps:get(X, MovesUp, X);
next_key_weird_keyboard(X, "D") ->
  MovesUp = #{
    2  => 6,
    6  => $A,
    1  => 3,
    3  => 7,
    7  => $B,
    $B => $D,
    4  => 8,
    8  => $C},
  maps:get(X, MovesUp, X).

%% Inputs

day_1a_input() ->
  "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1".

day_2a_input() ->
  "RLRDDRLLDLRLUDDULLDRUUULDDLRLUDDDLDRRDUDDDLLURDDDLDDDRDURUDRDRRULUUDUDDRRRLRRRRRLRULRLLRULDRUUDRLRRURDDRLRULDLDULLLRULURRUULLRLLDDDDLLDURRUDLDLURDRDRDLUUUDDRDUUDDULLUURRDRLDDULURRRUDLLULULDLLURURUDRRRRUDRLRDLRRLDDRDDLULDLLLURURDUDRRRRUULURLRDULDRLUDRRUDDUULDURUDLDDURRRDLULLUUDRLLDUUDLDRUDDRLLLLLLDUDUDDLRDLRRDRUDDRRRLLRRDLLRLDDURUURRRDDLDUULLDLDLRURDLLLDDRUUDRUDDDDULRLLDUULRUULLLULURRRLLULDLDUDLDLURUDUDULLDLLUUDRRDRLUURURURURDLURUUDLDRLUDDUUDULDULULLLDLDDULLULLDULRRDRULLURRRULLDDDULULURLRDURLLURUDDULLRUDLRURURRDRDUULDRUUDURDURDDLRDUUULDUUDRDURURDRRRURLLDDLLLURURULULUDLRDLDRDRURLRLULRDLU
UDLDURRULDRDDLDUULUDLDUULUURDDRUDRURRRUDRURLLDDRURLDLRDUUURDLLULURDDUDDDRRRURLLDLDLULRDULRLULDLUUDLLRLDLRUUULDDUURDLDDRRDLURLDUDDRURDRRURDURRRLUULURDDLRDLDRRRLDUDRLRLLRLDDUULDURUUULLLRRRRRRRDRRRDRLUULDLDDLULDRDUDLLUDRRUDRUUDULRLUURDDDDRRUUDLURULLLURDULUURDRDDURULRUDRRDLRDUUUUUDDDRDRDDRUDRDDDRLRUUDRDRDDDLUDRDRLDRDDRULURDRLDRUDUDRUULRLLUDRDRLLLLDUDRRLLURDLLLDRRUDDUDRLRLDUDRLURRUUULURDDRUURRLDRLRRRUUDLULDDDRDLDUUURLLUULDDRRUDLDDRUDUDUURURDDRDULLLLLULRRRDLRRRDDDLURDDDDLUULLLRDDURRRRLURRLDDLRUULULRDRDDDDLDUUUUUUDRRULUUUDD
UURDRRUDLURRDDDLUDLRDURUDURDLLLLRDLRLRDDRDRDUUULRDLLDLULULRDUDDRRUUDURULDLUDLRDRUDLDDULLLDDRDLLDULLLURLLRDDLDRDULRRDDULRDURLLRUDRLRRLUDURLDRDLDLRLLLURLRRURDLDURDLUDULRDULLLDRDDRDLDRDULUULURDRRRLDRRUULULLDDRRLDLRUURLRUURLURRLLULUUULRLLDDUDDLRLDUURURUDLRDLURRLLURUDLDLLUDDUULUUUDDDURDLRRDDDLDRUDRLRURUUDULDDLUUDDULLDDRRDDRRRUDUDUDLDLURLDRDLLLLDURDURLRLLLUUDLRRRRUDUDDLDLRUURRLRRLUURRLUDUDRRRRRRRLDUDDRUDDLUDLRDDDRLDUULDRDRRDLDRURDLDRULRLRLUDRDLRRUURUUUUDLDUUULLLRRRRRDLRRURDDLLLLUULDLLRULLUDLLDLLUDLRLRRLRURDDRRL
URDRDLLRDDDLLLDDLURLRURUURRRLUURURDURRLLUDURRLRLDLUURDLULRRDRUDDLULDLDRLDLRLRRLLLDDDUDDDLRURURRLLDRRRURUDLRDDLLDULDDLDRLUUUDRRRULDUULRDDDLRRLLURDDURLULRDUDURRLLDLLRLDUDDRRDDLRLLLDUDRLUURRLLDULRLDLUUUUUDULUDLULUDDUURRURLDLDRRLDLRRUDUDRRDLDUDDLULLDLLRDRURDRDRRLDDDDRDDRLLDDDLLUDRURLURDRRRRRUDDDUDUDDRDUUDRRUDUDRLULDDURULUURUUUURDRULRLRULLDDRRRUULRRRRURUDLDLRDLLDRLURLRUULLURDUDULRRURLRLLRRLLLURULRRRLDDUULLUUULRRDRULUUUUDRDRRDLRURLRLLRLRRRDRDRLDLUURUURULLDLULRRLRRDRULRRLLLDDURULLDLDLDLUUURDLDLUUDULRLLUDDRRDLLDLDLDURLUURRDDRRURDRLUDRLUUUDLDULDLUDRLDUDDLLRUDULLLLLDRRLLUULLUUURRDDUURDLLRDDLRLLU
LDUDRRDLUUDDRLLUUULURLDUDLUDLRLDRURLULRLLDDLRRUUUDDDDRDULDDUUDLRUULDRULLRDRUDDURLDUUURRUDUDRDRDURRDLURRRDRLDLRRRLLLRLURUURRDLLRDLDDLLRDUDDRDUULRULRRURLUDDUDDDUULLUURDULDULLLLRUUUDDRRRLDDDLDLRRDRDRDLUULRLULDRULDLRDRRUDULUDLLUDUULRDLRRUUDDLLDUDDRULURRLULDLDRRULDDRUUDDLURDLRDRLULRRLURRULDUURDLUDLLDRLDULLULDLLRDRDLLLUDLRULLRLDRDDDLDDDLRULDLULLRUUURRLLDUURRLRLDUUULDUURDURRULULRUUURULLLRULLURDDLDRLLRDULLUDLDRRRLLLLDUULRRLDURDURDULULDUURLDUDRLRURRDLUUULURRUDRUUUDRUR".
