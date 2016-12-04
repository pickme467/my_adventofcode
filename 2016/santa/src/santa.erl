-module(santa).

-export([day_1b/0, apply_directions/1]).

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

traverse(_Step, {Start, []}) ->
  {Start, []};
traverse(Step, {Start, History}) ->
  Steps = walk(Start, Step),
  find_new_point_or_crossing(Step, Start, History, Steps).

find_new_point_or_crossing(Step, Start, History, Steps) ->
  case find_crossing(Steps, History) of
    {true, Point} ->
      {{new_direction(Step, Start), Point}, []};
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

day_1a_input() ->
  "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1".
