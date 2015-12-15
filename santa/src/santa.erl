-module(santa).

-export([day_1a/0,
         day_1b/0,
         day_2a/0,
         day_2b/0,
         day_3a/0,
         day_3b/0,
         day_4a/0,
         day_4b/0,
         day_5a/0,
         day_5b/0,
         day_6a/0,
         day_6b/0,
         day_7a/0,
         day_7b/0,
         day_8a/0,
         day_8b/0,
         day_9a/0,
         day_9b/0,
         day_10a/0,
         day_10b/0,
         day_11a/0,
         day_11b/0,
         day_12a/0,
         day_12b/0,
         day_13a/0,
         day_13b/0,
         day_14a/0,
         day_14b/0,
         day_15a/0,
         day_15b/0]).

day_1a() ->
  day_1a(day_1_input(), 0).

day_1a([], Number) ->
  Number;
day_1a([$(  | Tail], Number) ->
  day_1a(Tail, Number + 1);
day_1a([$)  | Tail], Number) ->
  day_1a(Tail, Number - 1).

day_1b() ->
  day_1b(day_1_input(), 0, 0).

day_1b(_Input, -1, Step) ->
  Step;
day_1b([], _Floor, _Step) ->
  not_found;
day_1b([$( | Tail], Number, Step) ->
  day_1b(Tail, Number + 1, Step + 1);
day_1b([$) | Tail], Number, Step) ->
  day_1b(Tail, Number -1, Step + 1).

day_2a() ->
  day_2a(fun calculate_paper/2, day_2_input(), [], [], 0).

day_2a(Calculation, [], Element, Input, Sum) ->
  Calculation(Input ++ [list_to_integer(Element)], Sum);
day_2a(Calculation, [$\n | Tail], Element, Input, Sum) ->
  day_2a(Calculation, Tail, [], [],
         Calculation(Input ++ [list_to_integer(Element)], Sum));
day_2a(Calculation, [$x | Tail], Element, Input, Sum) ->
  day_2a(Calculation, Tail, [], Input ++ [list_to_integer(Element)], Sum);
day_2a(Calculation, [A | Tail], Element, Input, Sum) ->
  day_2a(Calculation, Tail, Element ++ [A], Input, Sum).

calculate_paper([A, B, C], Sum) ->
  Sum + 2 * A * B + 2 * A * C + 2 * B * C + lists:min([A*B, B*C, A*C]).

day_2b() ->
  day_2a(fun calculate_ribbon/2, day_2_input(), [], [], 0).

calculate_ribbon(List, Sum) ->
  [X, Y, Z] = lists:sort(List),
  Sum + X + X + Y + Y + X * Y * Z.

day_3a() ->
  day_3a(day_3_input(), {0, 0}, [{0, 0}]).

day_3a([], _Last, Visited) ->
  length(Visited);
day_3a([$< | Tail], {X, Y}, Visited) ->
  day_3a(Tail, {X - 1, Y}, update_visited_list({X - 1, Y}, Visited));
day_3a([$> | Tail], {X, Y}, Visited) ->
  day_3a(Tail, {X + 1, Y}, update_visited_list({X + 1, Y}, Visited));
day_3a([$^ | Tail], {X, Y}, Visited) ->
  day_3a(Tail, {X, Y + 1}, update_visited_list({X, Y + 1}, Visited));
day_3a([$v | Tail], {X, Y}, Visited) ->
  day_3a(Tail, {X, Y - 1}, update_visited_list({X, Y - 1}, Visited)).

update_visited_list(Value, List) ->
  case lists:member(Value, List) of
    true -> List;
    false -> List ++ [Value]
  end.

day_3b() ->
  day_3b(day_3_input(), {0, 0}, {0, 0}, [{0,  0}]).

day_3b([], _LastOne, _LastTwo, Visited) ->
  length(Visited);
day_3b([$< | Tail], {X, Y}, Other, Visited) ->
  day_3b(Tail, Other, {X - 1, Y}, update_visited_list({X - 1, Y}, Visited));
day_3b([$> | Tail], {X, Y}, Other, Visited) ->
  day_3b(Tail, Other, {X + 1, Y}, update_visited_list({X + 1, Y}, Visited));
day_3b([$^ | Tail], {X, Y}, Other, Visited) ->
  day_3b(Tail, Other, {X, Y + 1}, update_visited_list({X, Y + 1}, Visited));
day_3b([$v | Tail], {X, Y}, Other, Visited) ->
  day_3b(Tail, Other, {X, Y - 1}, update_visited_list({X, Y - 1}, Visited)).

day_4a() ->
  day_4a("yzbqklnj", 1, fun check_five_zeros/1).

day_4a(Input, Number, Assess) ->
  case Assess(
         binary_to_list(
           crypto:hash(
             md5,
             Input
             ++ lists:flatten(io_lib:format("~p", [Number]))))) of
    true ->
      Number;
    false ->
      day_4a(Input, Number + 1, Assess)
  end.

check_five_zeros([0, 0, X | _Tail]) when X < 16 ->
  true;
check_five_zeros(_Else) ->
  false.

day_4b() ->
  day_4a("yzbqklnj", 1, fun check_six_zeros/1).

check_six_zeros([0, 0, 0, X | _Tail]) when X =/= 0 ->
  true;
check_six_zeros(_Else) ->
  false.

day_5a() ->
  day_5a(fun nice_word_first_attempt/1, day_5_input(), [], 0).

day_5a(Condition, [], Word, Number) ->
  Number + Condition(Word);
day_5a(Condition, [$\n | Tail], Word, Number) ->
  day_5a(Condition, Tail, [], Number + Condition(Word));
day_5a(Condition, [Letter | Tail], Word, Number) ->
  day_5a(Condition, Tail, Word ++ [Letter], Number).

nice_word_first_attempt(Word) ->
  case vowel_condition(Word) andalso twice_letter_condition(Word)
    andalso bad_string_condition(Word) of
    true -> 1;
    false -> 0
  end.

vowel_condition(Word) ->
  length(
    lists:filter(
      fun (Letter) ->
          lists:member(Letter, "aeiou")
      end, Word)) >= 3.

twice_letter_condition([]) ->
  false;
twice_letter_condition([A, A | _Tail]) ->
  true;
twice_letter_condition([_A | Tail]) ->
  twice_letter_condition(Tail).

bad_string_condition([]) ->
  true;
bad_string_condition([$a, $b | _Tail]) ->
  false;
bad_string_condition([$c, $d | _Tail]) ->
  false;
bad_string_condition([$p, $q | _Tail]) ->
  false;
bad_string_condition([$x, $y | _Tail]) ->
  false;
bad_string_condition([_Other | Tail]) ->
  bad_string_condition(Tail).

day_5b() ->
  day_5a(fun nice_word_second_attempt/1, day_5_input(), [], 0).

nice_word_second_attempt(Word) ->
  case pair_more_than_once(Word)
    andalso letter_repeated_with_other_between(Word) of
    true -> 1;
    false -> 0
  end.

pair_more_than_once(Word) ->
  pair_more_than_once(Word, [], []).

pair_more_than_once([], _AnyPrevious, _Any) ->
  false;
pair_more_than_once([A, A | Tail], PreviousDouble, FoundPairs) ->
  case lists:member([A, A], PreviousDouble) of
    true ->
      pair_more_than_once([A | Tail], [], FoundPairs);
    false ->
      case lists:member([A, A], FoundPairs) of
        true ->
          true;
        false ->
          pair_more_than_once([A | Tail], [[A, A]], FoundPairs ++ [[A, A]])
      end
  end;
pair_more_than_once([A, B | Tail], _Any, FoundPairs) ->
  case lists:member([A, B], FoundPairs) of
    true ->
      true;
    false ->
      pair_more_than_once([B | Tail], [], FoundPairs ++ [[A, B]])
  end;
pair_more_than_once([_Any | Tail], _AnyPrevious, FoundPairs) ->
  pair_more_than_once(Tail, [], FoundPairs).

letter_repeated_with_other_between([]) ->
  false;
letter_repeated_with_other_between([A, _X, A | _Tail]) ->
  true;
letter_repeated_with_other_between([_Any | Tail]) ->
  letter_repeated_with_other_between(Tail).

day_6a() ->
  sets:size(day_6a(string:tokens(day_6_input(), "\n"), sets:new())).

day_6a([], Set) ->
  Set;
day_6a([Line | Tail], Set) ->
  {Command, FromCoordinates,
   ToCoordinates} = get_command_and_coordinates(Line),
  day_6a(Tail, update_set(Command, Set, FromCoordinates, ToCoordinates)).

get_command_and_coordinates(Line) ->
  {Command, InputWithoutCommand} = get_command(Line),
  {From, To} = get_coordinates(InputWithoutCommand),
  {Command, From, To}.

get_command(Input) ->
  case lists:prefix("turn on ", Input) of
    true ->
      {turn_on, lists:subtract(Input, "turn on ")};
    false ->
      case lists:prefix("turn off", Input) of
        true ->
          {turn_off, lists:subtract(Input, "turn off ")};
        false ->
          {toggle, lists:subtract(Input, "toggle ")}
      end
  end.

get_coordinates(Input) ->
  Tokens = string:tokens(Input, ", "),
  A = list_to_integer(lists:nth(1, Tokens)),
  B = list_to_integer(lists:nth(2, Tokens)),
  C = list_to_integer(lists:nth(4, Tokens)),
  D = list_to_integer(lists:nth(5, Tokens)),
  {{A, B}, {C, D}}.

update_set(turn_on, Set, A, B) ->
  sets:union(Set, get_set(A, B));
update_set(turn_off, Set, A, B) ->
  sets:subtract(Set, get_set(A, B));
update_set(toggle, Set, A, B) ->
  ToToggle = get_set(A, B),
  sets:union(sets:subtract(Set, sets:intersection(Set, ToToggle)),
             sets:subtract(ToToggle, Set)).

get_set({X1, Y1}, {X2, Y2}) ->
  sets:from_list([Y*1000 + X || X <- lists:seq(X1, X2),
                                Y <- lists:seq(Y1, Y2)]).

day_6b() ->
  lists:sum(array:to_list(day_6b(string:tokens(day_6_input(), "\n"),
                                 array:new([{size, 1000000},
                                            {default, 0}])))).

day_6b([], Array) ->
  Array;
day_6b([Line | Tail], Array) ->
  {Command, FromCoordinates,
   ToCoordinates} = get_command_and_coordinates(Line),
  day_6b(Tail, update_lights(Command, Array, FromCoordinates, ToCoordinates)).

update_lights(turn_on, Array, A, B) ->
  change_by(sets:to_list(get_set(A, B)), Array, +1);
update_lights(turn_off, Array, A, B) ->
  change_by(sets:to_list(get_set(A, B)), Array, -1);
update_lights(toggle, Array, A, B) ->
  change_by(sets:to_list(get_set(A, B)), Array, +2).

change_by([], Array, _Incrementer) ->
  Array;
change_by([Index | Tail], Array, Incrementer) ->
  Value = array:get(Index, Array),
  NewValue = case Value == 0 andalso Incrementer < 0 of
               true -> Value;
               false -> Value + Incrementer
             end,
  change_by(Tail, array:set(Index, NewValue, Array), Incrementer).

day_7a() ->
  day_7a(string:tokens(day_7_input(), "\n"), maps:new()).

day_7a([], Map) ->
  Key = "a",
  {Value, _NewMap} = execute_operation_for_key(Key, Map),
  Value;
day_7a([Command | Tail], Map) ->
  Wire = get_wire(Command),
  Operation = get_operation(Command),
  day_7a(Tail, maps:put(Wire, Operation, Map)).

get_wire(Command) ->
  [_Sequence, Wire] = string:tokens(Command, ">"),
  string:strip(Wire).

get_operation(Command) ->
  [Operation, _Rest] = string:tokens(Command, "-"),
  Members = string:tokens(string:strip(Operation), " "),
  parse_operation(Members).

parse_operation([X, Operation, Y]) ->
  {translate_operation(Operation), [to_int(X), to_int(Y)]};
parse_operation([_Operation, X]) ->
  {fun my_not/1, [to_int(X)]};
parse_operation([Value]) ->
  to_int(Value).

to_int(Value)->
  case string:to_integer(Value) of
    {error, no_integer} -> Value;
    {IntegerValue, _Rest} -> IntegerValue
  end.

translate_operation("AND") ->
  fun my_and/2;
translate_operation("OR") ->
  fun my_or/2;
translate_operation("RSHIFT") ->
  fun my_shiftr/2;
translate_operation("LSHIFT") ->
  fun my_shiftl/2.

my_not(A) ->
  bnot(A).

my_and(A, B) ->
  A band B.

my_or(A, B) ->
  A bor B.

my_shiftr(A, B) ->
  A bsr B.

my_shiftl(A, B) ->
  A bsl B.

execute_operation_for_key(Key, Map) ->
  execute_operation(Key, maps:get(Key, Map), Map).

execute_operation(Key, {Operation, [Param]}, Map) when is_integer(Param) ->
  Value = Operation(Param),
  {Value, map:put(Key, Value, Map)};
execute_operation(Key, {Operation, [X, Y]}, Map) when is_integer(X),
                                                      is_integer(Y) ->
  Value = Operation(X, Y),
  {Value, map:put(Key, Value, Map)};
execute_operation(Key, {Operation, [Param]}, Map) ->
  {NewParam, NewMap} = execute_operation_for_key(Param, Map),
  Value = Operation(NewParam),
  {Value, maps:put(Key, Value, NewMap)};
execute_operation(Key, {Operation, [X, Y]}, Map) when is_integer(X) ->
  {NewY, NewMap} = execute_operation_for_key(Y, Map),
  Value = Operation(X, NewY),
  {Value, maps:put(Key, Value, NewMap)};
execute_operation(Key, {Operation, [X, Y]}, Map) when is_integer(Y) ->
  {NewX, NewMap} = execute_operation_for_key(X, Map),
  Value = Operation(NewX, Y),
  {Value, maps:put(Key, Value, NewMap)};
execute_operation(Key, {Operation, [X, Y]}, Map) ->
  {NewX, NewMapX} = execute_operation_for_key(X, Map),
  {NewY, NewMapY} = execute_operation_for_key(Y, NewMapX),
  Value = Operation(NewX, NewY),
  {Value, maps:put(Key, Value, NewMapY)};
execute_operation(_Key, Value, Map) when is_integer(Value) ->
  {Value, Map};
execute_operation(Key, Value, Map) ->
  {NewValue, NewMap} = execute_operation(Key, maps:get(Value, Map), Map),
  {NewValue, maps:put(Key, NewValue, NewMap)}.

day_7b() ->
  day_7a(string:tokens(day_7_input() ++ "\n 956 -> b", "\n"), maps:new()).

day_8a() ->
  day_8a(day_8_input(), 0).

day_8a([], Sum) ->
  Sum;
day_8a([String | Tail], Sum) ->
  FormattedLength = get_formated_length(String),
  RawLength = get_raw_length(String),
  day_8a(Tail, Sum + RawLength - FormattedLength).

get_formated_length(String) ->
  get_formated_length(String, 0).

get_formated_length([], Sum) -> Sum;
get_formated_length([$\\, $x, _A, _B | Tail], Sum) ->
  get_formated_length(Tail, Sum + 1);
get_formated_length([$\\, $\\ | Tail], Sum) ->
  get_formated_length(Tail, Sum + 1);
get_formated_length([$\\, $" | Tail], Sum) ->
  get_formated_length(Tail, Sum + 1);
get_formated_length([_Letter | Tail], Sum) ->
  get_formated_length(Tail, Sum + 1).

get_raw_length(String) ->
  length(String) + 2.

day_8b() ->
  day_8b(day_8_input(), 0).

day_8b([], Sum) -> Sum;
day_8b([String | Tail], Sum) ->
  RawLength = get_raw_length(String),
  RawRawLength = get_raw_raw_length(String),
  day_8b(Tail, Sum + RawRawLength - RawLength).

get_raw_raw_length(String) ->
  get_raw_raw_length(String, 0).

get_raw_raw_length([], Sum) ->
  Sum + 6;
get_raw_raw_length([$\\ | Tail], Sum) ->
  get_raw_raw_length(Tail, Sum + 2);
get_raw_raw_length([$" | Tail], Sum) ->
  get_raw_raw_length(Tail, Sum + 2);
get_raw_raw_length([_Letter | Tail], Sum) ->
  get_raw_raw_length(Tail, Sum + 1).

day_9a() ->
  day_9a(string:tokens(day_9_input(), "\n"),
         fun find_minimum/2).

day_9a(Directions, ReducingFunction) ->
  day_9a(Directions, maps:new(), sets:new(), ReducingFunction).

day_9a([], Map, AllPlaces, ReducingFunction) ->
  List = sets:to_list(AllPlaces),
  AllRoutes = find_all_routes(List, List, Map, []),
  ReducingFunction(AllRoutes , Map);
day_9a([Direction | Tail], Map, AllPlaces, ReducingFunction) ->
  Distance = get_distance(Direction),
  {From, To} = get_from_to(Direction),
  day_9a(Tail, maps:put({From, To}, Distance, Map),
         sets:add_element(From, sets:add_element(To, AllPlaces)),
         ReducingFunction).

get_distance(Direction) ->
  [_FromTo, Distance] = string:tokens(Direction, "="),
  list_to_integer(string:strip(Distance)).

get_from_to(Direction) ->
  [FromTo, _Distance] = string:tokens(Direction, "="),
  [From, "to", To] = string:tokens(FromTo, " "),
  {list_to_atom(From), list_to_atom(To)}.

find_minimum(List, Map) ->
  find_extremum(List, Map, fun min/2, 99999).

find_extremum(List, Map, Calculation, Extremum) ->
  lists:foldl(fun (GivenList, Accumulator) ->
                  Calculation(
                    Accumulator,
                    calculate_distance(lists:reverse(GivenList), Map, 0))
              end, Extremum, List).

find_all_routes([], _AllPlaces, _Map, FoundRoutes) ->
  FoundRoutes;
find_all_routes([From | Tail], AllPlaces, Map, FoundRoutes) ->
  NewFoundRoutes = find_routes_from(From, AllPlaces, Map, FoundRoutes),
  find_all_routes(Tail, AllPlaces, Map, NewFoundRoutes).

find_routes_from(StartingAt, AllPlaces, Map, FoundRoutes) ->
  AllowedNext = get_allowed_next(StartingAt, AllPlaces, Map, []),
  find_routes_from([StartingAt], AllPlaces -- [StartingAt], AllowedNext,
                   Map, FoundRoutes).

find_routes_from(Visited, [], _AllowedNext, _Map, FoundRoutes) ->
  [Visited | FoundRoutes];
find_routes_from(_Visited, _Remaining, [], _Map, FoundRoutes) ->
  FoundRoutes;
find_routes_from(Visited, Remaining, [NextAllowed | Tail], Map, FoundRoutes) ->
  FoundNewRoutes = find_routes_from([NextAllowed | Visited],
                                    Remaining -- [NextAllowed],
                                    get_allowed_next(NextAllowed,
                                                     Remaining, Map, []),
                                    Map, FoundRoutes),
  find_routes_from(Visited, Remaining, Tail, Map, FoundNewRoutes).

get_allowed_next(_From, [], _Map, Allowed) ->
  Allowed;
get_allowed_next(From, [To | Tail], Map, Allowed) ->
  case maps:is_key({From, To}, Map) orelse maps:is_key({To, From}, Map) of
    true ->
      get_allowed_next(From, Tail, Map, [To | Allowed]);
    false ->
      get_allowed_next(From, Tail, Map, Allowed)
  end.

calculate_distance([_Last], _Map, Distance) ->
  Distance;
calculate_distance([From, To | Tail], Map, Distance) ->
  calculate_distance([To | Tail], Map, Distance + get_distance(From, To, Map)).

get_distance(From, To, Map) ->
  case maps:is_key({From, To}, Map) of
    true -> maps:get({From, To}, Map);
    false -> maps:get({To, From}, Map)
  end.

day_9b() ->
  day_9a(string:tokens(day_9_input(), "\n"),
         fun find_maximum/2).

find_maximum(List, Map) ->
  find_extremum(List, Map, fun max/2, 0).

day_10a() ->
  length(make_it(40, day_10_input())).

make_it(Times, Input) ->
  lists:foldl(fun(_Times, String) ->
                  day_10a(String, [], [], maps:new())
              end,
              Input, lists:seq(1, Times)).

day_10a([], Value, Output, Map) ->
  {NewOutput, _NewMap} = update_output(Output, Value, Map),
  lists:reverse(lists:flatten(NewOutput));
day_10a([Value | Tail], [Value | OtherTail], Output, Map) ->
  day_10a(Tail, [Value, Value | OtherTail], Output, Map);
day_10a([Value | Tail], [], Output, Map) ->
  day_10a(Tail, [Value], Output, Map);
day_10a([Value | Tail], OtherValue, Output, Map) ->
  {NewOutput, NewMap} = update_output(Output, OtherValue, Map),
  day_10a(Tail, [Value], NewOutput, NewMap).

number_to_string(Number) ->
  lists:flatten(io_lib:format("~p", [Number])).

update_output(Output, [Value | Tail] = Sequence, Map) ->
  GeneratedSequence = maps:get(
                        Sequence, Map,
                        lists:flatten(
                          [Value,
                           lists:reverse(
                             number_to_string(length([Value | Tail])))])),
  {[GeneratedSequence | Output], maps:put(Sequence, GeneratedSequence, Map)}.

day_10b() ->
  length(make_it(50, day_10_input())).

day_11a() ->
  lists:reverse(find_new_password(lists:reverse(day_11_input()))).

find_new_password(Old) ->
  New = increase_password(Old),
  case evaluate_password(lists:reverse(New)) of
    true ->
      New;
    false ->
      find_new_password(New)
  end.

increase_password([$z | Tail]) ->
  [$a | increase_password(Tail)];
increase_password([A | Tail]) ->
  [A + 1 | Tail].

evaluate_password(Password) ->
  does_not_have_prohibited_letters(Password) andalso
    has_two_pairs(Password) andalso
    has_increasing_straight(Password).

does_not_have_prohibited_letters(Password) ->
  not lists:member($i, Password) orelse
    lists:member($o, Password) orelse
    lists:member($l, Password).

has_two_pairs(Password) ->
  has_two_pairs(Password, []).

has_two_pairs([], _Pair) ->
  false;
has_two_pairs([_A, _A | _Tail], [_B, _B]) ->
  true;
has_two_pairs([A, A | Tail], []) ->
  has_two_pairs(Tail, [A, A]);
has_two_pairs([_X | Tail], Pair) ->
  has_two_pairs(Tail, Pair).

has_increasing_straight([]) ->
  false;
has_increasing_straight([A, B, C | _Tail]) when B == A + 1, C == A + 2 ->
  true;
has_increasing_straight([_A | Tail]) ->
  has_increasing_straight(Tail).

day_11b() ->
  lists:reverse(find_new_password(lists:reverse(day_11a()))).

day_12a() ->
  lists:sum(get_numbers(day_12_input())).

get_numbers(String) ->
  get_numbers(String, [], []).

get_numbers([], [], List) ->
  List;
get_numbers([], Number, List) ->
  [list_to_integer(Number) | List];
get_numbers([A | Tail], Number, List)
  when A == $- orelse (A >= $0 andalso A =< $9) ->
  get_numbers(Tail, Number ++ [A], List);
get_numbers([_A | Tail], [], List) ->
  get_numbers(Tail, [], List);
get_numbers([_A | Tail], Number, List) ->
  get_numbers(Tail, [], [list_to_integer(Number) | List]).

day_12b() ->
  lists:sum(get_numbers(filter_out_red(day_12_input()))).

filter_out_red(List) ->
  filter_out_red(List, [], [], []).

filter_out_red([], [], [], NoRedList) ->
  lists:reverse(NoRedList);
filter_out_red([$:, $", $r, $e, $d, $" | Tail], [], RedRecord, NoRedList) ->
  filter_out_red(Tail, [], "\"der\":" ++ RedRecord, NoRedList);
filter_out_red([$:, $", $r, $e, $d, $" | Tail], [Record | RecordTail],
               [], NoRedList) ->
  filter_out_red(Tail, RecordTail, "\"der\":" ++ Record,
                 NoRedList);
filter_out_red([$:, $", $r, $e, $d, $" | Tail], Records, RedRecord,
               NoRedList) ->
  filter_out_red(Tail, Records, "\"der\":" ++ RedRecord,
                 NoRedList);
filter_out_red([${ | Tail], [], [], NoRedList) ->
  filter_out_red(Tail, [[${]], [], NoRedList);
filter_out_red([${ | Tail], Records, [], NoRedList) ->
  filter_out_red(Tail, [[${] | Records], [], NoRedList);
filter_out_red([${ | Tail], Record, RedRecord, NoRedList) ->
  filter_out_red(Tail, Record, [${] ++ RedRecord, NoRedList);
filter_out_red([$} | Tail], [Record | []], [], NoRedList) ->
  filter_out_red(Tail, [], [], [$} | Record] ++ NoRedList);
filter_out_red([$} | Tail], [Record, ParentRecord | RecordTail], [],
               NoRedList) ->
  filter_out_red(Tail, [[$} | Record] ++ ParentRecord | RecordTail],
                 [], NoRedList);
filter_out_red([$} | Tail], Records, RedRecord, NoRedList) ->
  case number_of_open_closed_braces(RedRecord) of
    1 ->
      filter_out_red(Tail, Records, [], NoRedList);
    _N ->
      filter_out_red(Tail, Records, [$} | RedRecord], NoRedList)
  end;
filter_out_red([Letter | Tail], [], [], NoRedList) ->
  filter_out_red(Tail, [], [], [Letter | NoRedList]);
filter_out_red([Letter | Tail], [Record | RecordTail], [], NoRedList) ->
  filter_out_red(Tail, [[Letter | Record] | RecordTail], [], NoRedList);
filter_out_red([Letter | Tail], Record, RedRecord, NoRedList) ->
  filter_out_red(Tail, Record, [Letter | RedRecord], NoRedList).

number_of_open_closed_braces(List) ->
  lists:foldl(fun (${, Counter) ->
                  Counter + 1;
                  ($}, Counter) ->
                  Counter -1;
                  (_Any, Counter) ->
                  Counter
              end, 0, List).

day_13a() ->
  GuestsMap = day_13a(string:tokens(day_13_input(), "\n"), maps:new()),
  find_best_seatting(GuestsMap).

day_13a([], GuestsMap) ->
  GuestsMap;
day_13a([Statement | Tail], GuestsMap) ->
  [Who, _Would, GainOrLose, Points, _Happiness, _Units,
   _By, _Sitting, _Next, _To, Whom] = string:tokens(Statement, " ."),
  day_13a(Tail, add_to_guests_list(Who, Whom,
                                   GainOrLose, Points, GuestsMap)).

add_to_guests_list(Who, Whom, GainOrLose, Points, GuestsMap) ->
  Preferences = case maps:is_key(Who, GuestsMap) of
                  true  -> maps:get(Who, GuestsMap);
                  false -> maps:new()
                end,
  NewPreferences = maps:put(Whom, make_points(GainOrLose, Points), Preferences),
  maps:put(Who, NewPreferences, GuestsMap).

make_points("gain", Points) ->
  list_to_integer(Points);
make_points("lose", Points) ->
  -list_to_integer(Points).

find_best_seatting(GuestsMap) ->
  AllConfigurations = all_configurations(maps:keys(GuestsMap)),
  calculate_and_return_best(AllConfigurations, GuestsMap).

all_configurations([]) ->
  [[]];
all_configurations(List) ->
  [[Head | Tail] ||  Head <- List,
                     Tail <- all_configurations(List -- [Head])].

calculate_and_return_best(SeatSetupLists, GuestsMap) ->
  Counted = lists:map(fun ([First | SeatSetup]) ->
                          score([First | SeatSetup] ++ [First], 0, GuestsMap)
                      end, SeatSetupLists),
  lists:max(Counted).

score([_Last], Score, _Map) ->
  Score;
score([One, Two | Tail], Score, Map) ->
  score([Two | Tail], Score + maps:get(Two, maps:get(One, Map))
        + maps:get(One, maps:get(Two, Map)), Map).

day_13b() ->
  GuestsMap = day_13a(string:tokens(day_13_input(), "\n"), maps:new()),
  GuestsMapWithMe = add_to_guests_list("me", 0, GuestsMap),
  find_best_seatting(GuestsMapWithMe).

add_to_guests_list(Who, Score, Map) ->
  AllGuests = maps:keys(Map),
  HisMap = add_others_to_his_map(Score, AllGuests),
  add_him_to_others_map(Who, Score, AllGuests,
                        maps:put(Who, HisMap, Map)).

add_him_to_others_map(Who, Score, AllGuests, Map) ->
  lists:foldl(fun (Guest, GivenMap) ->
                  maps:put(Guest,
                           maps:put(Who, Score,
                                    maps:get(Guest, GivenMap)), GivenMap)
              end, Map, AllGuests).

add_others_to_his_map(Score, Others) ->
  lists:foldl(fun (Whom, Map) ->
                  maps:put(Whom, Score, Map)
              end,
              maps:new(), Others).

day_14a() ->
  lists:max(lists:map(
              fun ({_Name, Speed, RunTime, RestTime}) ->
                  get_distance(2503, run, Speed, RunTime, RestTime, 0)
              end, make_reineers())).

make_reineers() ->
  lists:map(fun (Reineer) ->
                [Name, _Can, _Fly, Distance,
                 _Kmh, _For, Seconds, _Seconds,
                 _But, _Then, _Must, _Rest, _For,
                 RestTime, _SecondsWithDot] =
                  string:tokens(Reineer, " "),
                {Name,
                 list_to_integer(Distance),
                 list_to_integer(Seconds),
                 list_to_integer(RestTime)}
            end, string:tokens(day_14_input(), "\n")).

get_distance(RemainingTime, stop, _Speed, _RunTime, RestTime, Distance) when
    RemainingTime =< RestTime -> Distance;
get_distance(RemainingTime, stop, Speed, RunTime, RestTime, Distance) ->
  get_distance(RemainingTime - RestTime, run,
               Speed, RunTime, RestTime, Distance);
get_distance(RemainingTime, run, Speed, RunTime, _RestTime, Distance) when
    RemainingTime =< RunTime ->
  Distance + RemainingTime * Speed;
get_distance(RemainingTime, run, Speed, RunTime, RestTime, Distance) ->
  get_distance(RemainingTime - RunTime, stop,
               Speed, RunTime, RestTime, Distance + RunTime * Speed).

day_14b() ->
  handle_race(2503, lists:map(fun (Reineer) ->
                                  {0, 0, 1, run, Reineer}
                              end, make_reineers())).

handle_race(0, Reineers) ->
  lists:max(lists:map(fun ({_Distance, Score,
                            _Step, _CurrentAction,
                            _Reineer}) ->
                          Score end, Reineers));
handle_race(Counter, Reineers) ->
  handle_race(Counter - 1, update_score(Reineers)).

update_score(Reineers) ->
  add_point_to_winners(
    lists:map(
      fun ({CurrentDistance, Score, Step, run,
            {_Name, Distance, Step, _RestTime} = Reineer}) ->
          {CurrentDistance + Distance, Score, 1, stop, Reineer};
          ({CurrentDistance, Score, Step, run,
            {_Name, Distance, _RunTime, _RestTime} = Reineer}) ->
          {CurrentDistance + Distance, Score, Step + 1, run, Reineer};
          ({CurrentDistance, Score, Step, stop,
            {_Name, _Distance, _RunTime, Step} = Reineer}) ->
          {CurrentDistance, Score, 1, run, Reineer};
          ({CurrentDistance, Score, Step, stop,
            {_Name, _Distance, _RunTime, _RestTime} = Reineer}) ->
          {CurrentDistance, Score, Step + 1, stop, Reineer} end,
      Reineers)).

add_point_to_winners(Reineers) ->
  [{Distance, _Score, _Step, _Action, _Reineer} = First | Rest] =
    lists:reverse(lists:sort(Reineers)),
  lists:map(fun ({ReineerDistance, ReineerScore, Step, Action, Reineer}) when
                  ReineerDistance == Distance ->
                {ReineerDistance, ReineerScore + 1, Step, Action, Reineer};
                (OtherScore) -> OtherScore end, [First | Rest]).

day_15a() ->
  find_best_cookie(score_all_recipies(), fun highest_score/1).

score_all_recipies() ->
  combine(get_ingredients(
            string:tokens(day_15_input(), "\n"))).

get_ingredients(List) ->
  lists:map(fun (Ingredient) ->
                [Name, _Capacity, CapValue,
                 _Durability, DurValue,
                 _Flavor, FlavValue,
                 _Texture, TextValue,
                 _Calories, CalValue] = string:tokens(Ingredient, " "),
                {Name -- ":", #{capacity => list_to_integer(CapValue -- ","),
                                durability => list_to_integer(DurValue -- ","),
                                flavor => list_to_integer(FlavValue -- ","),
                                texture => list_to_integer(TextValue -- ","),
                                calories => list_to_integer(CalValue)}}
            end, List).

combine(List) ->
  lists:foldl(fun(K, Acc) ->
                  Acc ++ (combine(K, List)) end,
              [], lists:seq(1, length(List))).

combine(1, L) -> [[X] || X <-L];
combine(K, L) when K == length(L) -> [L];
combine(K, [H|T]) ->
  [[H | Subcombos] || Subcombos <- combine(K-1, T)]
    ++(combine(K, T)).

find_best_cookie(List, EvaluationFunction) ->
  lists:max(lists:map(fun(Recipe) ->
                          find_best_recipe(Recipe, EvaluationFunction) end, List)).

find_best_recipe(Recipe, Evaluation) ->
  Multipiers = get_spoon_sets(length(Recipe)),
  lists:max(lists:map(fun (MultiplierList) ->
                          traverse_recipes(MultiplierList, Recipe,
                                           #{capacity => 0,
                                             durability => 0,
                                             flavor => 0,
                                             texture => 0,
                                             calories => 0}, Evaluation)
                      end, Multipiers)).

get_spoon_sets(N) ->
  get_multipliers(0, N, N).

get_multipliers(Higher, 1, _MaxN) when Higher < 100 ->
  [[100 - Higher]];
get_multipliers(_Higher, 1, _MaxN) ->
  [[100]];
get_multipliers(Higher, N, MaxN) ->
  [[L | T] || L <- lists:seq(1, 100 - MaxN + 1),
              T <- get_multipliers(L + Higher, N - 1, MaxN),
              lists:sum([L | T]) == 100 - Higher].

traverse_recipes([], [], Result, Evaluation) ->
  Evaluation(Result);
traverse_recipes([Number | TailNumbers],
                 [{_Name, Values} | TailRecipies],
                 RecipeScore, Evaluation) ->
  traverse_recipes(TailNumbers, TailRecipies,
                   lists:foldl(fun (Key, Map) ->
                                   maps:put(Key,
                                            Number * maps:get(Key, Values)
                                            + maps:get(Key, Map, 0), Map)
                               end, RecipeScore, maps:keys(RecipeScore)),
                   Evaluation).

highest_score(Result) ->
  case  is_non_negative(Result) of
    false -> 0;
    true ->
      score_recipe(Result)
  end.

is_non_negative(Result) ->
  lists:foldl(fun(Key, Value) ->
                  Value and (maps:get(Key, Result) > 0) end,
              true, maps:keys(Result)).

score_recipe(Result) ->
  lists:foldl(fun (calories, Value) -> Value;
                  (Key, Value) ->
                  Value * maps:get(Key, Result) end, 1, maps:keys(Result)).

day_15b() ->
  find_best_cookie(score_all_recipies(), fun highest_score_500_calories/1).

highest_score_500_calories(Result) ->
  Ha500Calories = maps:get(calories, Result) == 500,
  case  is_non_negative(Result) andalso Ha500Calories of
    false -> 0;
    true ->
      score_recipe(Result)
  end.

day_1_input() ->
    "()(((()))(()()()((((()(((())(()(()((((((()(()(((())))((()(((()))((())(()((()()()()(((())(((((((())))()()(()(()(())(((((()()()((())(((((()()))))()(())(((())(())((((((())())))(()())))()))))()())()())((()()((()()()()(()((((((((()()())((()()(((((()(((())((())(()))()((((()((((((((())()((()())(())((()))())((((()())(((((((((((()()(((((()(()))())(((()(()))())((()(()())())())(()(((())(())())()()(()(()((()))((()))))((((()(((()))))((((()(()(()())())()(((()((((())((((()(((()()(())()()()())((()((((((()((()()))()((()))()(()()((())))(((()(((()))((()((()(()))(((()()(()(()()()))))()()(((()(((())())))))((()(((())()(()(())((()())))((((())))(()(()(()())()((()())))(((()((()(())()()((()((())(()()((())(())()))()))((()(())()))())(((((((()(()()(()(())())))))))(()((((((())((((())((())())(()()))))()(())(()())()())((())(()))))(()))(()((()))()(()((((((()()()()((((((((()(()(())((()()(()()))(())()())()((())))()))()())(((()))(())()(())()))()((()((()(()()())(())()()()((())())))((()()(()()((()(())()()())(((()(()()))))(())))(()(()())()))()()))))))()))))((((((())))())))(()(())())(()())))))(()))()))))))()((()))))()))))(()(()((()())())(()()))))(((())()))())())())(((()(()()))(())()(())(())((((((()()))))((()(()))))))(()))())(((()()(()))()())()()()())))))))))))))(())(()))(()))((()(())(()())(())())(()())(())()()(()())))()()()))(())())()))())())(())((())))))))(())))(())))))()))))((())(()(((()))))(()))()((()(())))(()())(((((()))()())()()))))()))))()))())(()(()()()))()))))))((()))))))))))()((()))((()(())((())()()(()()))()(()))))()()(()))()))(((())))(())()((())(())(()())()())())))))))())))()((())))()))(()))()()))(((((((()))())(()()))(()()(()))()(()((()())()))))))(((()()()())))(())()))()())(()()))()()))))))))(())))()))()()))))))()))()())))()(())(())))))()(())()()(()()))))())((()))))()))))(()(((((()))))))))())))())()(())()()))))(())))())()()())()()())()(()))))()))()))))))))())))((()))()))()))())))()())()()())))())))(()((())()((()))())))))())()(())((())))))))))))())()())(())())())(()))(()))()))())(()(())())()())()()(()))))(()(())))))))(())))())(())))))))())()()(())())())))(())))))()))()(()())()(()))())())))))()()(()))()))))())))))))))()))))()))))))())()())()()))))()())))())))))))))))()()))))()()(((()))()()(())()))))((()))))(()))(())())))(())()))))))(()))()))))(())())))))()))(()())))))))))))))())))))))))()((()())(()())))))))((()))))(())(())))()(()())())))())())(()()()())))()))))))())))))())()()())))))))))))()()(()))))()())()))((()())(()))))()(()))))))))))()())())(((())(()))))())()))()))()))))))()))))))(()))))()))))()(())))(())))(()))())()()(()()))()))(()()))))))))()))(()))())(()()(()(()())()()))()))))))))(())))))((()()(()))())())))))()))())(()())()()))())))()(()()()()))((())())))())()(()()))()))))))))(()))(())))()))))(()(()())(()))))()())())()))()()))())))))))))))())()))))))()))))))))())))))()))))())(()())))(())()))())())))))()()(()()())(()())))()()))(((()))(()()()))))()))))()))))((())))()((((((()()))))))())))))))))))(((()))))))))))))(())())))))())(()))))))(()))((()))())))()(()((()))()))()))))))))))())()))()(()()))))())))())(())()(()))()))())(()))()))))(()()))()()(())))))()))(())(()(()()))(()()())))))(((()))))))()))))))))))))(())(()))))()())())()()((()()))())))))(()))))())))))))()()()))))))))())))()(((()()))(())))))(((())())))))((()))()(()))(()))))(()())))(()))())))))()))))(())(())))()((()))(())())))()()))()))))))))()))(()()()(()()()(()))())(())()())(((()))(())))))))))(((()())))()()))))))))()(())(()))()((((())(())(()())))()))(((())()()()))((()))(()))())())))())))(()))())()())())(()(())())()()()(())))())(())))(())))(())()))()))(()((()))))))))())(()))))))())(()()))()()))()(()(()())))()()(()((()((((((()))(())))()()()))())()))((()()(()))())((()(()(()))(()()))))()())))()))()())))))))()()((()())(())))()))(()))(())(()))())(()(())))()()))))))(((()(((()()))()(()(())())((()()))()))()))()))()(()()()(()))((()())()(())))()()))(((())()()())(())()((()()()()(()(())(()()))()(((((()())))((())))))(()()()))))(((()(())))()))((()((()(())()(()((())))((()())()(()))(((()())()()(()))(())(((()((()())()((())()())(((()()))((()((())(()))(()())(()()()))((()))(())(()((()()())((()))(())))(())(())(())))(()())))(((((()(()(((((()())((((()(()())(())(()()(((())((()(((()()(((()()((((((())))())(()((((((()(()))()))()()((()((()))))()(()()(()((()()))))))(((((()(((((())()()()(())())))))))()))((()()(())))(())(()()()())))))(()((((())))))))()()(((()(()(()(()(()())()()()(((((((((()()())()(()))((()()()()()(((((((()())()((())()))((((((()(()(()(()())(((()(((((((()(((())(((((((((())(())())()))((()(()))(((()()())(())(()(()()(((()(())()))())))(())((((((())(()()())()()(((()(((())(()(((())(((((((()(((((((((()))(())(()(()(()))))((()))()(())())())((()(()((()()))((()()((()(())(())(()((())(((())(((()()()((((((()()(())((((())()))))(())((()(()((())))(((((()(()()())())((())())))((())((()((()()((((((())(((()()(()())())(()(()))(()(()))())())()(((((((()(((()(())()()((())((()(()()((()(()()(((((((((((())((())((((((())((()((((()(()((((()(((((((())()((()))))())()((()((((()(()(((()((()())))(())())(((()(((())((((((()(((((((((()()(())))(()(((((()((((()())))((()((()((()(()()(((())((((((((((((()(((())(()(((((()))(()()(()()()()()()((())(((((((())(((((())))))())()(()()(()(()(((()()(((((())(()((()((()(((()()((()((((())()))()((((())(())))()())(((())(())(()()((()(((()()((((((((((()()(()())())(((((((((())((((()))()()((((())(()((((()(((())())(((((((((((()((((())))(())(()(((()(((()((())(((((()((()()(()(()()((((((()((((()((()(()((()(()((((((()))))()()(((((()((()(()(())()))(())(((((((()((((()())(()((()((()(()))())))(())((()))))(((((((()()()())(()))(()()((()())()((()((()()()(()(()()))(()())(())(((((()(((((((((((()((()(((()(((((((()()((((((()(((((()(()((()(((((())((((((()))((((())((()()((())(((())()(((((()()(((((()((()(()(((((((()(((((()((()((()((())(())((())(()))()()))(()()(()(()()(((((((()(((()(((())()(((((()((((((()())((((())()((()((()(()()())(()))((((()()((((((()((()(()(()((((()((()((())((((((()(()(())((((((()((((((((((()((())()))()(()(()(((((()()()))((())))()(()((((((((((((((()(((()((((()((())((()((()(((()()(()(((()((())(()()())))()(()(()(((((()()(()(()((((()(((((())()(()(()))(((((()()(((()()(())((((((((((((((())((())(((((((((((())()()()(())()(()(()(((((((((())(((()))(()()())(()((((()(())(((((()())(())((((((((())()((((()((((((())(()((()(())(((()((((()))(((((((((()()))((((()(())()()()(())(()((())((()()))()(((())(((((())((((((()()))(((((((((()((((((())))(((((((()((()(()(())))())(()(()))()(((((()())(()))()(()(())(((()))))())()())))(((((()))())()((()(()))))((()()()((((((()))()()((((((((())((()(()(((()(()((())((()())(()((((())(()(((()()()(()(()()))())())((((((((((())())((()))()((())(())(())))())()(()()(())))())(()))(((()(()()(((()(((())))()(((()(())()((((((())()))()))()((((((()(()(((((()())))()))))())()()(((()(((((())((()()(()((()((()(()(()(())))(()()()()((()(())(((()((()))((((()))())(())))())(()))()()()())()))(((()()())()((())))(())(()()()()(()())((()(()()((((())))((()((()(())((()(()((())()(()()(((()())()()())((()))((())(((()()(())))()()))(((()((())()(((((()())(())((())()())())((((((()(()(((((()))(()(".

day_2_input() ->
  "3x11x24
13x5x19
1x9x27
24x8x21
6x8x17
19x18x22
10x9x12
12x2x5
26x6x11
9x23x15
12x8x17
13x29x10
28x18x6
22x28x26
1x5x11
29x26x12
8x28x29
27x4x21
12x7x16
7x4x23
15x24x8
15x14x2
11x6x29
28x19x9
10x3x1
5x20x13
10x25x1
22x17x7
16x29x3
18x22x8
18x11x19
21x24x20
4x7x17
22x27x12
1x26x6
5x27x24
29x21x3
25x30x2
21x26x2
10x24x27
10x16x28
18x16x23
6x5x26
19x12x20
6x24x25
11x20x7
4x8x5
2x13x11
11x17x1
13x24x6
22x29x16
4x24x20
10x25x10
12x29x23
23x27x12
11x21x9
13x2x6
15x30x2
8x26x24
24x7x30
22x22x8
29x27x8
28x23x27
13x16x14
9x28x20
21x4x30
21x20x20
11x17x30
9x14x22
20x2x6
10x11x14
1x8x23
23x19x19
26x10x13
21x12x12
25x7x24
1x28x17
20x23x9
2x24x27
20x24x29
1x3x10
5x20x14
25x21x3
15x5x22
14x17x19
27x3x18
29x23x19
14x21x19
20x8x3
22x27x12
24x15x18
9x10x19
29x25x28
14x22x6
4x19x28
4x24x14
17x19x17
7x19x29
28x8x26
7x20x16
11x26x29
2x18x3
12x7x18
11x15x21
24x7x26
2x22x23
2x30x5
1x19x8
15x29x10
15x26x22
20x16x14
25x29x22
3x13x19
1x12x30
3x15x27
19x9x11
30x8x21
26x12x20
11x17x19
17x25x1
19x24x12
30x6x20
11x19x18
18x15x29
18x8x9
25x15x5
15x6x26
13x27x19
23x24x12
3x15x28
17x10x10
15x4x7
15x27x7
21x8x11
9x18x2
7x20x20
17x23x12
2x19x1
7x26x26
13x23x8
10x3x12
11x1x9
1x11x19
25x14x26
16x10x15
7x6x11
8x1x27
20x28x17
3x25x9
30x7x5
17x17x4
23x25x27
23x8x5
13x11x1
15x10x21
22x16x1
12x15x28
27x18x26
25x18x5
21x3x27
15x25x5
29x27x19
11x10x12
22x16x21
11x8x18
6x10x23
21x21x2
13x27x28
2x5x20
23x16x20
1x21x7
22x2x13
11x10x4
7x3x4
19x2x5
21x11x1
7x27x26
12x4x23
12x3x15
25x7x4
20x7x15
16x5x11
1x18x26
11x27x10
17x6x24
19x13x16
6x3x11
4x19x18
16x15x15
1x11x17
19x11x29
18x19x1
1x25x7
8x22x14
15x6x19
5x30x18
30x24x22
11x16x2
21x29x19
20x29x11
27x1x18
20x5x30
12x4x28
3x9x30
26x20x15
18x25x18
20x28x28
21x5x3
20x21x25
19x27x22
8x27x9
1x5x15
30x6x19
16x5x15
18x30x21
4x15x8
9x3x28
18x15x27
25x11x6
17x22x15
18x12x18
14x30x30
1x7x23
27x21x12
15x7x18
16x17x24
11x12x19
18x15x21
6x18x15
2x21x4
12x9x14
19x7x25
22x3x1
29x19x7
30x25x7
6x27x27
5x13x9
21x4x18
13x1x16
11x21x25
27x20x27
14x25x9
23x11x15
22x10x26
15x16x4
14x16x21
1x1x24
17x27x3
25x28x16
12x2x29
9x19x28
12x7x17
6x9x19
15x14x24
25x21x23
26x27x25
7x18x13
15x10x6
22x28x2
15x2x14
3x24x18
30x22x7
18x27x17
29x18x7
20x2x4
4x20x26
23x30x15
5x7x3
4x24x12
24x30x20
26x18x17
6x28x3
29x19x29
14x10x4
15x5x23
12x25x4
7x15x19
26x21x19
18x2x23
19x20x3
3x13x9
29x21x24
26x13x29
30x27x4
20x10x29
21x18x8
7x26x10
29x16x21
22x5x11
17x15x2
7x29x5
6x18x15
23x6x14
10x30x14
26x6x16
24x13x25
17x29x20
4x27x19
28x12x11
23x20x3
22x6x20
29x9x19
10x16x22
30x26x4
29x26x11
2x11x15
1x3x30
30x30x29
9x1x3
30x13x16
20x4x5
23x28x11
24x27x1
4x25x10
9x3x6
14x4x15
4x5x25
27x14x13
20x30x3
28x15x25
5x19x2
10x24x29
29x30x18
30x1x25
7x7x15
1x13x16
23x18x4
1x28x8
24x11x8
22x26x19
30x30x14
2x4x13
27x20x26
16x20x17
11x12x13
28x2x17
15x26x13
29x15x25
30x27x9
2x6x25
10x26x19
16x8x23
12x17x18
26x14x22
13x17x4
27x27x29
17x13x22
9x8x3
25x15x20
14x13x16
8x7x13
12x4x21
27x16x15
6x14x5
28x29x17
23x17x25
10x27x28
1x28x21
18x2x30
25x30x16
25x21x7
2x3x4
9x6x13
19x6x10
28x17x8
13x24x28
24x12x7
5x19x5
18x10x27
16x1x6
12x14x30
1x2x28
23x21x2
13x3x23
9x22x10
10x17x2
24x20x11
30x6x14
28x1x16
24x20x1
28x7x7
1x24x21
14x9x7
22x8x15
20x1x21
6x3x7
7x26x14
5x7x28
5x4x4
15x7x28
30x16x23
7x26x2
1x2x30
24x28x20
5x17x28
4x15x20
15x26x2
1x3x23
22x30x24
9x20x16
7x15x2
6x21x18
21x21x29
29x10x10
4x3x23
23x2x18
29x24x14
29x29x16
22x28x24
21x18x24
16x21x6
3x9x22
9x18x4
22x9x9
12x9x13
18x21x14
7x8x29
28x28x14
1x6x24
11x11x3
8x28x6
11x16x10
9x16x16
6x6x19
21x5x12
15x17x12
3x6x29
19x1x26
10x30x25
24x26x21
1x10x18
6x1x16
4x17x27
17x11x27
15x15x21
14x23x1
8x9x30
22x22x25
20x27x22
12x7x9
9x26x19
26x25x12
8x8x16
28x15x10
29x18x2
25x22x6
4x6x15
12x18x4
10x3x20
17x28x17
14x25x13
14x10x3
14x5x10
7x7x22
21x2x14
1x21x5
27x29x1
6x20x4
7x19x23
28x19x27
3x9x18
13x17x17
18x8x15
26x23x17
10x10x13
11x5x21
25x15x29
6x23x24
10x7x2
19x10x30
4x3x23
22x12x6
11x17x16
6x8x12
18x20x11
6x2x2
17x4x11
20x23x22
29x23x24
25x11x21
22x11x15
29x3x9
13x30x5
17x10x12
10x30x8
21x16x17
1x5x26
22x15x16
27x7x11
16x8x18
29x9x7
25x4x17
10x21x25
2x19x21
29x11x16
18x26x21
2x8x20
17x29x27
25x27x4
14x3x14
25x29x29
26x18x11
8x24x28
7x30x24
12x30x22
29x20x6
3x17x1
6x15x14
6x22x20
13x26x26
12x2x1
7x14x12
15x16x11
3x21x4
30x17x29
9x18x27
11x28x16
22x3x25
18x15x15
2x30x12
3x27x22
10x8x8
26x16x14
15x2x29
12x10x7
21x20x15
2x15x25
4x14x13
3x15x13
29x8x3
7x7x28
15x10x24
23x15x5
5x7x14
24x1x22
1x11x13
26x4x19
19x16x26
5x25x5
17x25x14
23x7x14
24x6x17
5x13x12
20x20x5
22x29x17
11x17x29
25x6x4
29x8x16
28x22x24
24x23x17
16x17x4
17x8x25
22x9x13
24x4x8
18x10x20
21x23x21
13x14x12
23x26x4
4x10x29
2x18x18
19x5x21
2x27x23
6x29x30
21x9x20
6x5x16
25x10x27
5x29x21
24x14x19
19x11x8
2x28x6
19x25x6
27x1x11
6x8x29
18x25x30
4x27x26
8x12x1
7x17x25
7x14x27
12x9x5
14x29x13
18x17x5
23x1x3
28x5x13
3x2x26
3x7x11
1x8x7
12x5x4
2x30x21
16x30x11
3x26x4
16x9x4
11x9x22
23x5x6
13x20x3
4x3x2
14x10x29
11x8x12
26x15x16
7x17x29
18x19x18
8x28x4
22x6x13
9x23x7
11x23x20
13x11x26
15x30x13
1x5x8
5x10x24
22x25x17
27x20x25
30x10x21
16x28x24
20x12x8
17x25x1
30x14x9
14x18x6
8x28x29
12x18x29
9x7x18
6x12x25
20x13x24
22x3x12
5x23x22
8x10x17
7x23x5
10x26x27
14x26x19
10x18x24
8x4x4
16x15x11
3x14x9
18x5x30
29x12x26
16x13x12
15x10x7
18x5x26
14x1x6
10x8x29
3x4x9
19x4x23
28x17x23
30x7x17
19x5x9
26x29x28
22x13x17
28x2x1
20x30x8
15x13x21
25x23x19
27x23x1
4x6x23
29x29x24
5x18x7
4x6x30
17x15x2
27x4x2
25x24x14
28x8x30
24x29x5
14x30x14
10x18x19
15x26x22
24x19x21
29x23x27
21x10x16
7x4x29
14x21x3
21x4x28
17x16x15
24x7x13
21x24x15
25x11x16
10x26x13
23x20x14
20x29x27
14x24x14
14x23x12
18x6x5
3x18x9
8x18x19
20x26x15
16x14x13
30x16x3
17x13x4
15x19x30
20x3x8
13x4x5
12x10x15
8x23x26
16x8x15
22x8x11
12x11x18
28x3x30
15x8x4
13x22x13
21x26x21
29x1x15
28x9x5
27x3x26
22x19x30
4x11x22
21x27x20
22x26x7
19x28x20
24x23x16
26x12x9
13x22x9
5x6x23
20x7x2
18x26x30
3x6x28
24x18x13
28x19x16
25x21x25
25x19x23
22x29x10
29x19x30
4x7x27
5x12x28
8x26x6
14x14x25
17x17x2
5x27x11
8x2x2
3x20x24
26x10x9
22x28x27
18x15x20
12x11x1
5x14x30
7x3x16
2x16x16
18x20x15
13x14x29
1x17x12
13x5x23
19x4x10
25x19x11
15x17x14
1x28x27
11x9x28
9x10x18
30x11x22
21x21x20
2x1x5
2x25x1
7x3x4
22x15x29
21x28x15
12x12x4
21x30x6
15x10x7
10x14x6
21x26x18
14x25x6
9x7x11
22x3x1
1x16x27
1x14x23
2x13x8
14x19x11
21x26x1
4x28x13
12x16x20
21x13x9
3x4x13
14x9x8
21x21x12
27x10x17
6x20x6
28x23x23
2x28x12
8x10x10
3x9x2
20x3x29
19x4x16
29x24x9
26x20x8
15x28x26
18x17x10
7x22x10
20x15x9
6x10x8
7x26x21
8x8x16
15x6x29
22x30x11
18x25x8
6x21x20
7x23x25
8x25x26
11x25x27
22x18x23
3x2x14
16x16x1
15x13x11
3x9x25
29x25x24
9x15x1
12x4x1
23x30x20
3x1x23
6x10x29
28x13x24
4x19x17
6x6x25
27x29x17
12x13x2
10x7x13
14x15x8
22x2x3
27x17x19
23x10x16
5x9x25
9x25x14
11x18x6
18x10x12
9x4x15
7x16x14
17x24x10
11x4x6
12x9x17
22x18x12
6x24x24
6x22x23
5x17x30
6x9x5
17x20x10
6x8x12
14x17x13
29x10x17
22x4x5
10x19x30
22x29x11
10x12x29
21x22x26
16x6x25
1x26x24
30x17x16
27x28x5
30x13x22
7x26x12
11x24x30
1x17x25
22x1x3
29x24x6
4x8x24
13x9x20
8x12x9
21x25x4
23x23x28
5x2x19
29x3x15
22x1x14
3x23x30
8x25x3
15x8x14
30x14x6
23x27x24
19x1x2
10x9x13
13x8x7
8x13x22
5x15x20
17x14x8
5x11x20
5x10x27
24x17x19
21x2x3
15x30x26
21x19x15
2x7x23
13x17x25
30x15x19
26x4x10
2x25x8
9x9x10
2x25x8
19x21x30
17x26x12
7x5x10
2x22x14
10x17x30
1x8x5
23x2x25
22x29x8
13x26x1
26x3x30
25x17x8
25x18x26
26x19x15
8x28x10
12x16x29
30x6x29
28x19x4
27x26x18
15x23x17
5x21x30
8x11x13
2x26x7
19x9x24
3x22x23
6x7x18
4x26x30
13x25x20
17x3x15
8x20x18
23x18x23
28x23x9
16x3x4
1x29x14
20x26x22
3x2x22
23x8x17
19x5x17
21x18x20
17x21x8
30x28x1
29x19x23
12x12x11
24x18x7
21x18x14
14x26x25
9x11x3
10x7x15
27x6x28
14x26x4
28x4x1
22x25x29
6x26x6
1x3x13
26x22x12
6x21x26
23x4x27
26x13x24
5x24x28
22x16x7
3x27x24
19x28x2
11x13x9
29x16x22
30x10x24
14x14x22
22x23x16
14x8x3
20x5x14
28x6x13
3x15x25
4x12x22
15x12x25
10x11x24
7x7x6
8x11x9
21x10x29
23x28x30
8x29x26
16x27x11
1x10x2
24x20x16
7x12x28
28x8x20
14x10x30
1x19x6
4x12x20
18x2x7
24x18x17
16x11x10
1x12x22
30x16x28
18x12x11
28x9x8
23x6x17
10x3x11
5x12x8
22x2x23
9x19x14
15x28x13
27x20x23
19x16x12
19x30x15
8x17x4
10x22x18
13x22x4
3x12x19
22x16x23
11x8x19
8x11x6
7x14x7
29x17x29
21x8x12
21x9x11
20x1x27
1x22x11
5x28x4
26x7x26
30x12x18
29x11x20
3x12x15
24x25x17
14x6x11".

day_3_input() ->
  ">^^v^<>v<<<v<v^>>v^^^<v<>^^><^<<^vv>>>^<<^>><vv<<v^<^^><>>><>v<><>^^<^^^<><>>vv>vv>v<<^>v<>^>v<v^<>v>><>^v<<<<v^vv^><v>v^>>>vv>v^^^<^^<>>v<^^v<>^<vv^^<^><<>^>><^<>>><><vv><>v<<<><><>v><<>^^^^v>>^>^<v<<vv^^<v<^<^>^^v^^^^^v<><^v><<><^v^>v<<>^<>^^v^<>v<v^>v>^^<vv^v><^<>^v<><^><v^><><><<<<>^vv^>^vvvvv><><^<vv^v^v>v<<^<^^v^<>^<vv><v<v^v<<v<<^^>>^^^v^>v<><^vv<<^<>v<v><><v^^><v<>^^>^^>v^>^<<<<v><v<<>v><^v>^>><v^^<^>v<vvvv<>>>>>^v^^>v<v<^<vv>^>^vv^>vv^^v<<^<^^<>v>vv^v>><>>>v^>^>^^v<>^<v<<>^vv>v^<<v>v<<><v>^vvv<v<vvv^v<vv<v^^^>v><<^<>><v^^>^v^>>^v<^<><v<>>v^<>>v<>>v^^^><^>>vvvv>^v<^><<>>^<>^>vv><v<<>>^^>v^^^><^<<^^v>v<^<<>v>^^vvv^v^>v^<>^^<>v^v>v>v<v^>vv>^^v<>v>>^<>><>v>v^<<vvvv<vvv><v^<^>^v<>>^><v>><>^<v>v<v>vv^>>vvv<>v>v<v^>>^>>v<<>^<>^<>>>^v<<<^<^v>vv^>><<><v^>^v^^^v<>^^vv><>><>>^>v^<v<>v<>>^<<^v>^^^<>^v^><>v<<v>vv^>vv<<>>><<^v^<>v<vv>>>^^<>^><<^>vv>>^<<v^^vv<>>><v>v><^<v<<>>>^^<>>^<^v><>vv^^^v>vvv>^><<>^^>^<<v^<v<^v<<>vvv<^<<>^>^v<vv<^>vvv>v>vv^<v^><>>^vv<^^^vv><^vv<v^<><v^vvv><<^>^^><v<<vv^>v<vv<v>^<>^v<<>v<v^v^>^>^>v<<^vvv<<<v>^^>^<<<<>vv>>^<>^>>>v<v>^^<v^<v<>>>vv>^^v<<>>>^^v><<<v<v<^v<>^^><v<^v<<v^><><^<><v<^^v>>><v^^v<<v^><^<><<v^>><^<>v>v^<><^<v>^v^>^>^vv^>^^<<vv^>vv<^vvv<>>^^<^>v^>^>^<v^><v<v>>>v<<<><^v<<><^<vv^v^^^>v<^^<v^vvv<v<><v<vv<^vv<>vv<v^<>>vvvvv<<>^v^v>vv>>>vvv^^<^<^<><>v<v>><^v><^<<<>><<<v>^>v<>^>^v>>^<>v^<^>><<>^<v>^>^^^>^^<v>>>><>^v^v><<<<vv^<vv<>vv>v<>v^<v^>v><>>>v^<><^vvv>vv^<^<<^<^^v>^>>>v<^<^v^^<^<^>>><v>vv>^<<><>^>>v>^<<>><^<>v<>vv^^>^>vvv^v<<^<^^<vv<>^vvv<^^v^vv^>>v<^>^^<v^<>v<^<^vv>v<<vv>vv>^>vvv>>>^^>v<>^v>v^<^>>v>^^v>>>>v^<v>v<^>v<v<<>>^v<^^<v><^<>>^<<vv^>>v<<v>^v<>><^>vv<v<^>>^^<vvvvvvvvv>>>v<v<>v^<>>^vv<v^^v<<^vvv^<<^><>vv<><<>>v>vv^><>>^^v^>>v^v^><<<>>^^<^v<<^<>>>>^<^>v^><<^>v<^v<^>>^^<<<<><^<^v^v<>>^v<^<<vv^<><^^vv><v^v^v>^>>^>^vv^>^v<v^v<<vvv^><>>^v^^><>v>vv><^>>vv<vvv<<<<^<>vvv^v<v>^<v<^>^<^<v<><>v^^^^<<vv<^^vv<v>><<v^><>>><v^>^v><^>^><vv^<><^<v>><<^vv<>>v^<<v<>v><v<><><vv>^>>v^<^<v>^><>>><^><v^v<>>>^^<^>v<v>vvv<>^<<><v^^>^>>v<^v>^>v>>>vv>v>>v^^^<^<vvv^<>^>^<v^<v^v>v>^>vv>vvv<>v<^>v>^^>>^<vv^^v>v^^^^^v^vv><^<><>^>vv<^>>^vvvv^^^>^<vv>^v<<^><^^>^<>^^>^<<v<^>>>^><<^^>v^v>>^>vvvv>^^v><v>>vv><<<vv<^>v>^^^<v>v^vvv<^><<^>^<>^><<<<<v^<<vv^v>^<>v<v>^>^>><>v^v<^vv^^>vv<<v^v>vv^vvv<<<<>^v<v^^v^v>v<<v>^^<>^vv^^>^>^v^vv^>>v^vv^^<vv><<v^v^^v><vv<^vvv<vv^^<<v>v^v^^^^v<^<^>v>^>v>^vv^v^^<v<^vvvv<<<>^<^^^<^^<>^<><vv<^^<<^>>><v^vvvv>^<>>^^>v^^v^<<v^^^<<<><^<v^v^^v<v^<>v><<v<>^v>v<^><^>vv^^<vvv<^v>>v>^<><v^><^^^<v^>>vv<<<<<^<>^v^v>^vv^<>v>v<^>vv<<^vv>vv<v<><>>v>><v<^<^^>><<v^v<<^><v<^<vv<v<<vv^>^<<><^^>^<^>>^<vv>><v<<vvv<^^v^>^^<^v>^v<v<>v><v^v^<<^<><<v<<^v>v<<>>^>v>>v>>v<^<<^<^>>>v>^^^v><^>^^>>v<<>^v><v>vvv^vv<<<>vvv<<>^>>>v<v<v^<^<^>^<^>v^^v<^^<v<>v<>>^^>^v^>v<<<<^<>v^><<<v>>>><<v^<^vv>v>><>>^<<<^<^^>v<>>v<>vv<<^<<><<^>v^^^vv^>vvvv>>v>v^><<v<>vv^<<><<vvv>^>>>^<<<^<^<<v>^>v<>>v>>vv^^><<<<^^^v>><<^><v><v^^><v<<v^^v^^v>>v<><><<>^><v><^<vv>><^v<>v<vvv<>^>><v>>v<^><<v>^<>^v><^><^^<v>^><^^v^<<><>>^>v^<^v^vv<><^>vv^>v^vvv^<>>^><^<^<>^<<v^v<^v><>^v<v>>^>>^v^vv>><vv><v^^<<^v^<>^v<<>^><^>><v>>v<<<v^^vv<>^^v>>><><><<v^<<<v^<^^><v^>v^^vv<v^<>>vv^<^v<>^v>>v^v>v<^^vv><>^v<<>v^<>v^>>v>vvv<^><><^^>^vv^>>v^>^<^^<><>><<>^^^><^v^v><<<><<^v^vv>v>><^>>><v^>v<v><><v^v<>v^^>>v<<>v>v<v<v<^^<><>v^^<>>v<^v<v>v<><v<v>^<<>v>vv^^<>>^^^<>^^>^v>v>>>^v^v><v^^<><v>^^v^v<^<^^><<v<^<^<>^<>><<>^>>^>^^><v><>v<><>><<<>>>>vv>>>^>>^v<^>v^^^v<<vv>><<<^<<<>>>>>^>vv<^v^<>^<v^>^v><v>vvv<>>>^v^^^v<<<<>>^^<vv<^<^^>^<>v<^<<<>><>>v<^<>^<vvv<^<>><><<v>^^^>^^<<v<v^>^^v^>><<^vv><v>^v>>^<v>v>^^>^v>^vvv<>v^v^^<><vv>vv^>>><>v<^><v<v^<><<<>^v>^v<<<^>^>^>v^v<<><vvv<<v^^<><v>^>>><vv>><v>>v^<vv>>vv<<^v^v<<><^v<vv>>>vv<>>>>^vv>v^<>vv>v^v<v^><v<^^^^^>vv<><<vvv^<v><^<vv><^^^vv^<>^^^^<^><^<>v^<v^v<<^v<<^^<>>^<v^^>>>vv<vvv<>v<<>><^vvv^<<^^<<>>>^<>>>v^^><>><<>><v^v>>>>>><>>><v^<<vvv^>v<>>v^<>vv<><^^^^v^<<^<v^vv><<^^>v<^vvv^v>>v>^>>v>^^><<v^<>v<>vv<^v^vv><v><<vv^v>>v^>>v<^^^>^><<v<>^><>v>>>vvv<v<vv<^>>^v<v>^<^^^^^v><>v><>v^v^v<v^vv^v>vvvv<>vv<<<vv<v<<>^<^>^^v^<<>^<v><^><v<v<><<>v^<<^<><vv>v<<^v>>^v<><v>^>>^^><>v^<^<vvv^>^>^<<<<>vv>^v^v<^^^<vv>><>^^<<v<^<^^>>>v^v<<^^^<v<v<^<>^v<v><v^vv^^v^^v^^<vv<>^<><vv^<^v^<<^><<vvv>^^<^^^<^v>^>^vv><<<^v<v>vv>v<>v^v<v^>v^>>>v^v<>^v<<>^vv>v>v>v^<^>v^^<^>^^^^vv>^^><^>vv^>>^^v>><<<<^><>v<>^<v<vv^>^^><<^><v>v^>^^<^>>><>><v^v<v^<v<vv^v^<<^<vvv>>><vv<^^>>^>^><<v^<>>v>v^v^^><<>vv^v>v^<v><^<>^^<^>v>^<><<<v>^<^<^>^>^>^^v^<<^^v^^<^<>><^>v>>^^<>^^^<<<<v^>^v<^vv>^<<<v<><<v<>vv>>>v><>>><>>v<<<vv><>^v>v<^>><^><><v<>^v^>^v>^v<<><<^<>>v>^><>^>><>><^<v^><v^^<><v><^^>^v^^<>v^<v^<^v<v^^^^^v^<<^>^^^<^v><>^^<<<><<<<<^^>v^vvvv>v<>>vv<^>^v^>v<^vv^v<<><<v>v^v>^^><><^<v^>v><vv><>>><<>^vv<>v>>v<^v>>>v<v>v>v>^vv<<>^^vv<v<^v^<v<v>vv<>^<^<vv<v^<^v^^><<>^>><^v>vv^^v<<^^><<>v^^<><><v^^<v^v>^>^>^>v<^<v>^v^^>v<>vvv<^v<v^v><<v^><<^^><^<<v^v^>v<>^>v><><v>^<v<v>^<^^^>^v<<><<><>vv>v^<>v^><v^v<v><><<v>v<vv><<v>>v>^<<<>vv>>vvv>^^vv^v^^<^^<>v^^<>v>>^^>^>^>v>><^>><>>^<<>><^>v<<<<<<<^v^v<v^<v^^>^<><<v<^>v^>v^vv<<^^vv^>>>>^<>v<^v<>v<vv<^>>v^vv>vv><vv<<^>v>><vv>>>vv^<<<<vv^>v<<<<^^>^^v^><<^<v^>v^>^^<v<>vvv^>^<>vvv<v<^^>v^<<v>><>v<v<>^^<vvv>^>vv><><<<^^vv<v^<v<>v<>><<v><^vv^>^<^>^^^<<<v>vv^<^<<>^>^<vv>v><v<<^><^>^^<vv^v^^>>>>vv^><^^vv><>^<v^v>v<vv>v><<<v>v<v>^><v^^><v>v<^v^>>^^<v^>^^>vv>>vv^><^vv^vv<<^>vv>^v<v><vv><v<vvvvv>^^v^v><v>>>^vv<>v>^^^^<^>><>^v^^^>v<^^<<^^v<vv<>vvv<^>><><^>>^><^<>v<v<<><<v><v^v<>><^>v><<v^<v>v<^<vv^v^v^>vvv^^>v>^<vv^>v^v^<>v>^>>vv>><^^<v<<>^vv<><><<^v<v>v<<vv><>><^v<v>>v^>vvv^v^<<^><v<>^vv^>v^<v<^>>v<v><v><v>>^<<<v^<><<>v>^>^^<v<>>^<>^>^><<<^<<^<<^>^v>>><vvv>><<<<v>>>>>>>^<^v<^>v<>vv<><>v>>^>>^>vv^^><<^<v<v>>^^<<^>v<^>>vv>^<>v><^>v<vv>>>>>>^v<^<<<v^><vv<<>>vv<<><v<><<<v<^<v<>>v<^^^^v^^<^^^<^<vv><<^>><>v<<>v<v<>>>><>v^vv>^>^>>vv^v<v<<><^v>vv^><v<<>v^v<^>vv<<^^v><^>>^^vv<^<>>v^^>><v>^v>>>^>>v>v<>v<^vv><>^<<^>vv>>><><>v^><>v^>v>v><^v<><v<v>^v<<^vv^><^^>><^^^<<<^>v>^v>>><^>><^>>>^^^<^>vv<><<<v^>^<^^>>^^^v^v^v>v<v>>>><^>>>v>^vv<<^^^<^^vv>v<<><v<<^^>v>><<v^^><^>^<^>^v^>v><^<^vv>v>><>^<<vv<<v>v<vv<v>^>^>><^^<v>^v^v<><<>vvv<^<v>^><>^>vvv>>>^><<>><v^^<^<<^v>>^v<v<vv>vv^v^>v<<vvv<^^v^v>^<^>>^>v<^>^v<<><<<^>^<^^^>vv<^^^^vv<v<^^v<<<<v<^v^<><v<<^><<>vv>>><^<^<>>>^>^>>^<<<<<^^v>^>^<>vvv^^<^><^>^^v>^vv^><v^<^<<v^<vvv<<^v<><^><^>>>v>^v>^>^v<vv^v>><v><^><v^^>v^>^<><<><>v<v^>vvv^>^>>v<>^><^>^><vvv>^^v^v>v<>^v^><^>>v>v^><<<^>>^<>^<>>v><>>v^>^>^^<>>v^>^<vvvv<^vvvv^>>vv^<v^v>^vv<>v<>^<v<v>v>^^><^>vv^<^v^<<^<^<><vv<^v<^v><>>>^v^<<^><^>vv<v>v<^>vv^>v<<<>^<><v<^^^>v><^^<>^<^<v^vv^<<^>><<v^v<^vvv<<<>>vvvv^v^^^>v<>>><<>vvv<<^^^>v>v>>v<<v<v^v^>^^v>^><^<><<v^<v<v^^^><>v^^^<v>vv<>^>^^vv>^<<^v<^v><v>>>^>>><^<<>^v>>^>vv<<<v<>^<v><v^<^<>v>v^^v^>><<^v<<<<>v>v>v^^<^><>^^<<<v>vv<>>>^>>v<><v^>^<><vv>v>v^v<v^<^>>^>><<^^<^^v<vv<>><<<v<^<<^^^>vvv^<vvv<^>vv><>><<<^<v^v^^<<^vvv^^<^<><<>^<^<>>vvv<>^<>v^v<><>>v^v><<>>>vvv>v<>^>>^><^>vv<<>>v<<^><>v>>^^<v>^>^<<>><^<<vv<^<vv^vv><>>>><^<v>^>vv<v><>^<>vvvvv^vv<<v<>>>^<<><>^^vvv>>>vv<<^^><^v^^v<>^^>^><^>v^^^^v<^<<vv<vv<>vv^^>v^vv>v><>>vv>^<^<v^v^>>v^v^^v>^>vv^>v<vvvv<^v<^v>^v>^^v<<^>^^<<>^><^v>>>vv^>^^>vvvv>>v<^<v>^>>>v^<><^<^^<v>vv^^><v>v^<>^^^>>><^^v>v>^<<>^<v^>vvv^>^^^><v<^>>v<v>>^v><<><<>v<^<<>^><>^>vv>^<v>^^v<<^v^vvv^^>^vv^<^>^>^^v>v^>^<<><<^>v>>vv^vv><v>>^<<^<v^^<^<v^^vv^><^^<^^><v^^>v^^^<^<>^<>>^v<^vvv^^v^<><^>>>>>v><><<<>vv<^v>><<>vvv<><<vv<<<^>v^^>>^>^v>><><^^v<>><>>v^>^<vv><<<>><><<v>^^<>>v<><^<vv>vv<^v>^<<<<v<^<<^^>>^<><^>><<>^>v>^^^v>>^<^^v><v^v>^><<><>>^>>^<<v<>^v<>^>^<v>>vv>^vvv<<v<<^>^>^<<^^<>^^^^vvv<>^vv<vvvvv^^>^^<^>>><>v^<><^<<^>v^^v<>>^vv<>v^^<>>v^vvvvv<<v^<v^^>>><vvvvv>><^>vv>v^v^<v<^>^^><^>^^^^v<><^v<<>v^>v>>vv<<>^<v^^>vvv>^^<v^<>vv^><>><v^^v<>^>>^>v><>>^^v>^>^>>>^>v<^v>v>^<^^^^^>>v<v<>>v<<^>^<v<<>^^>><<^><>v<>^^^vv<>^^>><<^^>v>vv>vv>v^>^v>v^^<>>><<v><v<<>>v><>vvv^^v>^^>^vvvv^>^<>^vvvv><v><v<>>><>^<^vv<>^v<^v<>^vvv<<>><vvv^>>^><<vv^<v^>^<v<<^^>^^<^^v^>v<>v^v><>><v^^>>^vvv><^vv>v^<^<^v>>v^^>^vvv^<v^^v^^>v<^<>>^<>>>^^<><^^vv<>^vv^<>>>>^^<<^^<>vv^^><>^^<v<<v>^<v^^>^v<><><>vvv>^v^>>vv<<^v<<>><v>^><^>>>^<^<^^>vv^<<^<>>^^><><<v>^^<v>>v<<vvvv>^v^vv>><^^<<^>>v>v<^^^<^><^^vv>^vv<^<vv<>v><^<><v><^^^>>^<><^<v>>>>v^<v>>>>>v<><^^>v<^<^>><v<>^>vv>^^v^v^<<v<><<<^v^><<^<><<<<v<^>><<<>v>>vv><vv<><<^<^<><vv>^^^^<>v<<<<v>vv<>vv^^^>><>vv^><>>^vv<<><^^vv<>v^>>^<<>^<v^<^>v<".

day_5_input() ->
  "sszojmmrrkwuftyv
isaljhemltsdzlum
fujcyucsrxgatisb
qiqqlmcgnhzparyg
oijbmduquhfactbc
jqzuvtggpdqcekgk
zwqadogmpjmmxijf
uilzxjythsqhwndh
gtssqejjknzkkpvw
wrggegukhhatygfi
vhtcgqzerxonhsye
tedlwzdjfppbmtdx
iuvrelxiapllaxbg
feybgiimfthtplui
qxmmcnirvkzfrjwd
vfarmltinsriqxpu
oanqfyqirkraesfq
xilodxfuxphuiiii
yukhnchvjkfwcbiq
bdaibcbzeuxqplop
ivegnnpbiyxqsion
ybahkbzpditgwdgt
dmebdomwabxgtctu
ibtvimgfaeonknoh
jsqraroxudetmfyw
dqdbcwtpintfcvuz
tiyphjunlxddenpj
fgqwjgntxagidhah
nwenhxmakxqkeehg
zdoheaxqpcnlhnen
tfetfqojqcdzlpbm
qpnxkuldeiituggg
xwttlbdwxohahwar
hjkwzadmtrkegzye
koksqrqcfwcaxeof
wulwmrptktliyxeq
gyufbedqhhyqgqzj
txpunzodohikzlmj
jloqfuejfkemcrvu
amnflshcheuddqtc
pdvcsduggcogbiia
yrioavgfmeafjpcz
uyhbtmbutozzqfvq
mwhgfwsgyuwcdzik
auqylgxhmullxpaa
lgelzivplaeoivzh
uyvcepielfcmswoa
qhirixgwkkccuzlp
zoonniyosmkeejfg
iayfetpixkedyana
ictqeyzyqswdskiy
ejsgqteafvmorwxe
lhaiqrlqqwfbrqdx
ydjyboqwhfpqfydc
dwhttezyanrnbybv
edgzkqeqkyojowvr
rmjfdwsqamjqehdq
ozminkgnkwqctrxz
bztjhxpjthchhfcd
vrtioawyxkivrpiq
dpbcsznkpkaaclyy
vpoypksymdwttpvz
hhdlruwclartkyap
bqkrcbrksbzcggbo
jerbbbnxlwfvlaiw
dwkasufidwjrjfbf
kkfxtjhbnmqbmfwf
vmnfziwqxmioukmj
rqxvcultipkecdtu
fhmfdibhtjzkiqsd
hdpjbuzzbyafqrpd
emszboysjuvwwvts
msyigmwcuybfiooq
druyksfnbluvnwoh
fvgstvynnfbvxhsx
bmzalvducnqtuune
lzwkzfzttsvpllei
olmplpvjamynfyfd
padcwfkhystsvyfb
wjhbvxkwtbfqdilb
hruaqjwphonnterf
bufjobjtvxtzjpmj
oiedrjvmlbtwyyuy
sgiemafwfztwsyju
nsoqqfudrtwszyqf
vonbxquiiwxnazyl
yvnmjxtptujwqudn
rrnybqhvrcgwvrkq
taktoxzgotzxntfu
quffzywzpxyaepxa
rfvjebfiddcfgmwv
iaeozntougqwnzoh
scdqyrhoqmljhoil
bfmqticltmfhxwld
brbuktbyqlyfpsdl
oidnyhjkeqenjlhd
kujsaiqojopvrygg
vebzobmdbzvjnjtk
uunoygzqjopwgmbg
piljqxgicjzgifso
ikgptwcjzywswqnw
pujqsixoisvhdvwi
trtuxbgigogfsbbk
mplstsqclhhdyaqk
gzcwflvmstogdpvo
tfjywbkmimyyqcjd
gijutvhruqcsiznq
ibxkhjvzzxgavkha
btnxeqvznkxjsgmq
tjgofgauxaelmjoq
sokshvyhlkxerjrv
ltogbivktqmtezta
uduwytzvqvfluyuf
msuckpthtgzhdxan
fqmcglidvhvpirzr
gwztkqpcwnutvfga
bsjfgsrntdhlpqbx
xloczbqybxmiopwt
orvevzyjliomkkgu
mzjbhmfjjvaziget
tlsdxuhwdmghdyjb
atoecyjhwmznaewi
pyxpyvvipbqibiox
ajbfmpqqobfsmesj
siknbzefjblnohgd
eqfhgewbblwdfkmc
opylbscrotckkrbk
lbwxbofgjkzdxkle
ceixfjstaptdomvm
hnkrqxifjmmjktie
aqykzeuzvvetoygd
fouahjimfcisxima
prkzhutbqsyrhjzx
qqwliakathnsbzne
sayhgqtlcqqidqhj
ygduolbysehdudra
zricvxhdzznuxuce
ucvzakslykpgsixd
udirhgcttmyspgsb
yuwzppjzfsjhhdzi
gtqergjiuwookwre
xvxexbjyjkxovvwf
mlpaqhnnkqxrmwmm
ezuqbrjozwuqafhb
mcarusdthcbsonoq
weeguqeheeiigrue
pngtfugozxofaqxv
copphvbjcmfspenv
jiyahihykjjkdaya
gdqnmesvptuyrfwp
vbdscfywqmfxbohh
crtrfuxyjypzubrg
seihvevtxywxhflp
fvvpmgttnapklwou
qmqaqsajmqwhetpk
zetxvrgjmblxvakr
kpvwblrizaabmnhz
mwpvvzaaicntrkcp
clqyjiegtdsswqfm
ymrcnqgcpldgfwtm
nzyqpdenetncgnwq
cmkzevgacnmdkqro
kzfdsnamjqbeirhi
kpxrvgvvxapqlued
rzskbnfobevzrtqu
vjoahbfwtydugzap
ykbbldkoijlvicbl
mfdmroiztsgjlasb
quoigfyxwtwprmdr
ekxjqafwudgwfqjm
obtvyjkiycxfcdpb
lhoihfnbuqelthof
eydwzitgxryktddt
rxsihfybacnpoyny
bsncccxlplqgygtw
rvmlaudsifnzhcqh
huxwsyjyebckcsnn
gtuqzyihwhqvjtes
zreeyomtngvztveq
nwddzjingsarhkxb
nuqxqtctpoldrlsh
wkvnrwqgjooovhpf
kwgueyiyffudtbyg
tpkzapnjxefqnmew
ludwccvkihagvxal
lfdtzhfadvabghna
njqmlsnrkcfhtvbb
cajzbqleghhnlgap
vmitdcozzvqvzatp
eelzefwqwjiywbcz
uyztcuptfqvymjpi
aorhnrpkjqqtgnfo
lfrxfdrduoeqmwwp
vszpjvbctblplinh
zexhadgpqfifcqrz
ueirfnshekpemqua
qfremlntihbwabtb
nwznunammfexltjc
zkyieokaaogjehwt
vlrxgkpclzeslqkq
xrqrwfsuacywczhs
olghlnfjdiwgdbqc
difnlxnedpqcsrdf
dgpuhiisybjpidsj
vlwmwrikmitmoxbt
sazpcmcnviynoktm
pratafauetiknhln
ilgteekhzwlsfwcn
ywvwhrwhkaubvkbl
qlaxivzwxyhvrxcf
hbtlwjdriizqvjfb
nrmsononytuwslsa
mpxqgdthpoipyhjc
mcdiwmiqeidwcglk
vfbaeavmjjemfrmo
qzcbzmisnynzibrc
shzmpgxhehhcejhb
wirtjadsqzydtyxd
qjlrnjfokkqvnpue
dxawdvjntlbxtuqc
wttfmnrievfestog
eamjfvsjhvzzaobg
pbvfcwzjgxahlrag
omvmjkqqnobvnzkn
lcwmeibxhhlxnkzv
uiaeroqfbvlazegs
twniyldyuonfyzqw
wgjkmsbwgfotdabi
hnomamxoxvrzvtew
ycrcfavikkrxxfgw
isieyodknagzhaxy
mgzdqwikzullzyco
mumezgtxjrrejtrs
nwmwjcgrqiwgfqel
wjgxmebfmyjnxyyp
durpspyljdykvzxf
zuslbrpooyetgafh
kuzrhcjwbdouhyme
wyxuvbciodscbvfm
kbnpvuqwmxwfqtqe
zddzercqogdpxmft
sigrdchxtgavzzjh
lznjolnorbuddgcs
ycnqabxlcajagwbt
bnaudeaexahdgxsj
rlnykxvoctfwanms
jngyetkoplrstfzt
tdpxknwacksotdub
yutqgssfoptvizgr
lzmqnxeqjfnsxmsa
iqpgfsfmukovsdgu
qywreehbidowtjyz
iozamtgusdctvnkw
ielmujhtmynlwcfd
hzxnhtbnmmejlkyf
ftbslbzmiqkzebtd
bcwdqgiiizmohack
dqhfkzeddjzbdlxu
mxopokqffisxosci
vciatxhtuechbylk
khtkhcvelidjdena
blatarwzfqcapkdt
elamngegnczctcck
xeicefdbwrxhuxuf
sawvdhjoeahlgcdr
kmdcimzsfkdfpnir
axjayzqlosrduajb
mfhzreuzzumvoggr
iqlbkbhrkptquldb
xcvztvlshiefuhgb
pkvwyqmyoazocrio
ajsxkdnerbmhyxaj
tudibgsbnpnizvsi
cxuiydkgdccrqvkh
cyztpjesdzmbcpot
nnazphxpanegwitx
uphymczbmjalmsct
yyxiwnlrogyzwqmg
gmqwnahjvvdyhnfa
utolskxpuoheugyl
mseszdhyzoyavepd
ycqknvbuvcjfgmlc
sknrxhxbfpvpeorn
zqxqjetooqcodwml
sesylkpvbndrdhsy
fryuxvjnsvnjrxlw
mfxusewqurscujnu
mbitdjjtgzchvkfv
ozwlyxtaalxofovd
wdqcduaykxbunpie
rlnhykxiraileysk
wgoqfrygttlamobg
kflxzgxvcblkpsbz
tmkisflhativzhde
owsdrfgkaamogjzd
gaupjkvkzavhfnes
wknkurddcknbdleg
lltviwincmbtduap
qwzvspgbcksyzzmb
ydzzkumecryfjgnk
jzvmwgjutxoysaam
icrwpyhxllbardkr
jdopyntshmvltrve
afgkigxcuvmdbqou
mfzzudntmvuyhjzt
duxhgtwafcgrpihc
tsnhrkvponudumeb
sqtvnbeiigdzbjgv
eczmkqwvnsrracuo
mhehsgqwiczaiaxv
kaudmfvifovrimpd
lupikgivechdbwfr
mwaaysrndiutuiqx
aacuiiwgaannunmm
tjqjbftaqitukwzp
lrcqyskykbjpaekn
lirrvofbcqpjzxmr
jurorvzpplyelfml
qonbllojmloykjqe
sllkzqujfnbauuqp
auexjwsvphvikali
usuelbssqmbrkxyc
wyuokkfjexikptvv
wmfedauwjgbrgytl
sfwvtlzzebxzmuvw
rdhqxuechjsjcvaf
kpavhqkukugocsxu
ovnjtumxowbxduts
zgerpjufauptxgat
pevvnzjfwhjxdoxq
pmmfwxajgfziszcs
difmeqvaghuitjhs
icpwjbzcmlcterwm
ngqpvhajttxuegyh
mosjlqswdngwqsmi
frlvgpxrjolgodlu
eazwgrpcxjgoszeg
bbtsthgkjrpkiiyk
tjonoglufuvsvabe
xhkbcrofytmbzrtk
kqftfzdmpbxjynps
kmeqpocbnikdtfyv
qjjymgqxhnjwxxhp
dmgicrhgbngdtmjt
zdxrhdhbdutlawnc
afvoekuhdboxghvx
hiipezngkqcnihty
bbmqgheidenweeov
suprgwxgxwfsgjnx
adeagikyamgqphrj
zzifqinoeqaorjxg
adhgppljizpaxzld
lvxyieypvvuqjiyc
nljoakatwwwoovzn
fcrkfxclcacshhmx
ownnxqtdhqbgthch
lmfylrcdmdkgpwnj
hlwjfbvlswbzpbjr
mkofhdtljdetcyvp
synyxhifbetzarpo
agnggugngadrcxoc
uhttadmdmhidpyjw
ohfwjfhunalbubpr
pzkkkkwrlvxiuysn
kmidbxmyzkjrwjhu
egtitdydwjxmajnw
civoeoiuwtwgbqqs
dfptsguzfinqoslk
tdfvkreormspprer
zvnvbrmthatzztwi
ffkyddccrrfikjde
hrrmraevdnztiwff
qaeygykcpbtjwjbr
purwhitkmrtybslh
qzziznlswjaussel
dfcxkvdpqccdqqxj
tuotforulrrytgyn
gmtgfofgucjywkev
wkyoxudvdkbgpwhd
qbvktvfvipftztnn
otckgmojziezmojb
inxhvzbtgkjxflay
qvxapbiatuudseno
krpvqosbesnjntut
oqeukkgjsfuqkjbb
prcjnyymnqwqksiz
vuortvjxgckresko
orqlyobvkuwgathr
qnpyxlnazyfuijox
zwlblfkoklqmqzkw
hmwurwtpwnrcsanl
jzvxohuakopuzgpf
sfcpnxrviphhvxmx
qtwdeadudtqhbely
dbmkmloasqphnlgj
olylnjtkxgrubmtk
nxsdbqjuvwrrdbpq
wbabpirnpcsmpipw
hjnkyiuxpqrlvims
enzpntcjnxdpuqch
vvvqhlstzcizyimn
triozhqndbttglhv
fukvgteitwaagpzx
uhcvukfbmrvskpen
tizcyupztftzxdmt
vtkpnbpdzsaluczz
wodfoyhoekidxttm
otqocljrmwfqbxzu
linfbsnfvixlwykn
vxsluutrwskslnye
zbshygtwugixjvsi
zdcqwxvwytmzhvoo
wrseozkkcyctrmei
fblgtvogvkpqzxiy
opueqnuyngegbtnf
qxbovietpacqqxok
zacrdrrkohfygddn
gbnnvjqmkdupwzpq
qgrgmsxeotozvcak
hnppukzvzfmlokid
dzbheurndscrrtcl
wbgdkadtszebbrcw
fdmzppzphhpzyuiz
bukomunhrjrypohj
ohodhelegxootqbj
rsplgzarlrknqjyh
punjjwpsxnhpzgvu
djdfahypfjvpvibm
mlgrqsmhaozatsvy
xwktrgyuhqiquxgn
wvfaoolwtkbrisvf
plttjdmguxjwmeqr
zlvvbwvlhauyjykw
cigwkbyjhmepikej
masmylenrusgtyxs
hviqzufwyetyznze
nzqfuhrooswxxhus
pdbdetaqcrqzzwxf
oehmvziiqwkzhzib
icgpyrukiokmytoy
ooixfvwtiafnwkce
rvnmgqggpjopkihs
wywualssrmaqigqk
pdbvflnwfswsrirl
jeaezptokkccpbuj
mbdwjntysntsaaby
ldlgcawkzcwuxzpz
lwktbgrzswbsweht
ecspepmzarzmgpjm
qmfyvulkmkxjncai
izftypvwngiukrns
zgmnyjfeqffbooww
nyrkhggnprhedows
yykzzrjmlevgffah
mavaemfxhlfejfki
cmegmfjbkvpncqwf
zxidlodrezztcrij
fseasudpgvgnysjv
fupcimjupywzpqzp
iqhgokavirrcvyys
wjmkcareucnmfhui
nftflsqnkgjaexhq
mgklahzlcbapntgw
kfbmeavfxtppnrxn
nuhyvhknlufdynvn
nviogjxbluwrcoec
tyozixxxaqiuvoys
kgwlvmvgtsvxojpr
moeektyhyonfdhrb
kahvevmmfsmiiqex
xcywnqzcdqtvhiwd
fnievhiyltbvtvem
jlmndqufirwgtdxd
muypbfttoeelsnbs
rypxzbnujitfwkou
ubmmjbznskildeoj
ofnmizdeicrmkjxp
rekvectjbmdnfcib
yohrojuvdexbctdh
gwfnfdeibynzjmhz
jfznhfcqdwlpjull
scrinzycfhwkmmso
mskutzossrwoqqsi
rygoebkzgyzushhr
jpjqiycflqkexemx
arbufysjqmgaapnl
dbjerflevtgweeoj
snybnnjlmwjvhois
fszuzplntraprmbj
mkvaatolvuggikvg
zpuzuqygoxesnuyc
wnpxvmxvllxalulm
eivuuafkvudeouwy
rvzckdyixetfuehr
qgmnicdoqhveahyx
miawwngyymshjmpj
pvckyoncpqeqkbmx
llninfenrfjqxurv
kzbjnlgsqjfuzqtp
rveqcmxomvpjcwte
bzotkawzbopkosnx
ktqvpiribpypaymu
wvlzkivbukhnvram
uohntlcoguvjqqdo
ajlsiksjrcnzepkt
xsqatbldqcykwusd
ihbivgzrwpmowkop
vfayesfojmibkjpb
uaqbnijtrhvqxjtb
hhovshsfmvkvymba
jerwmyxrfeyvxcgg
hncafjwrlvdcupma
qyvigggxfylbbrzt
hiiixcyohmvnkpgk
mmitpwopgxuftdfu
iaxderqpceboixoa
zodfmjhuzhnsqfcb
sthtcbadrclrazsi
bkkkkcwegvypbrio
wmpcofuvzemunlhj
gqwebiifvqoeynro
juupusqdsvxcpsgv
rbhdfhthxelolyse
kjimpwnjfrqlqhhz
rcuigrjzarzpjgfq
htxcejfyzhydinks
sxucpdxhvqjxxjwf
omsznfcimbcwaxal
gufmtdlhgrsvcosb
bssshaqujtmluerz
uukotwjkstgwijtr
kbqkneobbrdogrxk
ljqopjcjmelgrakz
rwtfnvnzryujwkfb
dedjjbrndqnilbeh
nzinsxnpptzagwlb
lwqanydfirhnhkxy
hrjuzfumbvfccxno
okismsadkbseumnp
sfkmiaiwlktxqvwa
hauwpjjwowbunbjj
nowkofejwvutcnui
bqzzppwoslaeixro
urpfgufwbtzenkpj
xgeszvuqwxeykhef
yxoldvkyuikwqyeq
onbbhxrnmohzskgg
qcikuxakrqeugpoa
lnudcqbtyzhlpers
nxduvwfrgzaailgl
xniuwvxufzxjjrwz
ljwithcqmgvntjdj
awkftfagrfzywkhs
uedtpzxyubeveuek
bhcqdwidbjkqqhzl
iyneqjdmlhowwzxx
kvshzltcrrururty
zgfpiwajegwezupo
tkrvyanujjwmyyri
ercsefuihcmoaiep
ienjrxpmetinvbos
jnwfutjbgenlipzq
bgohjmrptfuamzbz
rtsyamajrhxbcncw
tfjdssnmztvbnscs
bgaychdlmchngqlp
kfjljiobynhwfkjo
owtdxzcpqleftbvn
ltjtimxwstvzwzjj
wbrvjjjajuombokf
zblpbpuaqbkvsxye
gwgdtbpnlhyqspdi
abipqjihjqfofmkx
nlqymnuvjpvvgova
avngotmhodpoufzn
qmdyivtzitnrjuae
xfwjmqtqdljuerxi
csuellnlcyqaaamq
slqyrcurcyuoxquo
dcjmxyzbzpohzprl
uqfnmjwniyqgsowb
rbmxpqoblyxdocqc
ebjclrdbqjhladem
ainnfhxnsgwqnmyo
eyytjjwhvodtzquf
iabjgmbbhilrcyyp
pqfnehkivuelyccc
xgjbyhfgmtseiimt
jwxyqhdbjiqqqeyy
gxsbrncqkmvaryln
vhjisxjkinaejytk
seexagcdmaedpcvh
lvudfgrcpjxzdpvd
fxtegyrqjzhmqean
dnoiseraqcoossmc
nwrhmwwbykvwmgep
udmzskejvizmtlce
hbzvqhvudfdlegaa
cghmlfqejbxewskv
bntcmjqfwomtbwsb
qezhowyopjdyhzng
todzsocdkgfxanbz
zgjkssrjlwxuhwbk
eibzljqsieriyrzr
wamxvzqyycrxotjp
epzvfkispwqynadu
dwlpfhtrafrxlyie
qhgzujhgdruowoug
girstvkahaemmxvh
baitcrqmxhazyhbl
xyanqcchbhkajdmc
gfvjmmcgfhvgnfdq
tdfdbslwncbnkzyz
jojuselkpmnnbcbb
hatdslkgxtqpmavj
dvelfeddvgjcyxkj
gnsofhkfepgwltse
mdngnobasfpewlno
qssnbcyjgmkyuoga
glvcmmjytmprqwvn
gwrixumjbcdffsdl
lozravlzvfqtsuiq
sicaflbqdxbmdlch
inwfjkyyqbwpmqlq
cuvszfotxywuzhzi
igfxyoaacoarlvay
ucjfhgdmnjvgvuni
rvvkzjsytqgiposh
jduinhjjntrmqroz
yparkxbgsfnueyll
lyeqqeisxzfsqzuj
woncskbibjnumydm
lltucklragtjmxtl
ubiyvmyhlesfxotj
uecjseeicldqrqww
xxlxkbcthufnjbnm
lhqijovvhlffpxga
fzdgqpzijitlogjz
efzzjqvwphomxdpd
jvgzvuyzobeazssc
hejfycgxywfjgbfw
yhjjmvkqfbnbliks
sffvfyywtlntsdsz
dwmxqudvxqdenrur
asnukgppdemxrzaz
nwqfnumblwvdpphx
kqsmkkspqvxzuket
cpnraovljzqiquaz
qrzgrdlyyzbyykhg
opoahcbiydyhsmqe
hjknnfdauidjeydr
hczdjjlygoezadow
rtflowzqycimllfv
sfsrgrerzlnychhq
bpahuvlblcolpjmj
albgnjkgmcrlaicl
pijyqdhfxpaxzdex
eeymiddvcwkpbpux
rqwkqoabywgggnln
vckbollyhgbgmgwh
ylzlgvnuvpynybkm
hpmbxtpfosbsjixt
ocebeihnhvkhjfqz
tvctyxoujdgwayze
efvhwxtuhapqxjen
rusksgefyidldmpo
nkmtjvddfmhirmzz
whvtsuadwofzmvrt
iiwjqvsdxudhdzzk
gucirgxaxgcassyo
rmhfasfzexeykwmr
hynlxcvsbgosjbis
huregszrcaocueen
pifezpoolrnbdqtv
unatnixzvdbqeyox
xtawlpduxgacchfe
bdvdbflqfphndduf
xtdsnjnmzccfptyt
nkhsdkhqtzqbphhg
aqcubmfkczlaxiyb
moziflxpsfubucmv
srdgnnjtfehiimqx
pwfalehdfyykrohf
sysxssmvewyfjrve
brsemdzosgqvvlxe
bimbjoshuvflkiat
hkgjasmljkpkwwku
sbnmwjvodygobpqc
bbbqycejueruihhd
corawswvlvneipyc
gcyhknmwsczcxedh
kppakbffdhntmcqp
ynulzwkfaemkcefp
pyroowjekeurlbii
iwksighrswdcnmxf
glokrdmugreygnsg
xkmvvumnfzckryop
aesviofpufygschi
csloawlirnegsssq
fkqdqqmlzuxbkzbc
uzlhzcfenxdfjdzp
poaaidrktteusvyf
zrlyfzmjzfvivcfr
qwjulskbniitgqtx
gjeszjksbfsuejki
vczdejdbfixbduaq
knjdrjthitjxluth
jweydeginrnicirl
bottrfgccqhyycsl
eiquffofoadmbuhk
lbqfutmzoksscswf
xfmdvnvfcnzjprba
uvugkjbkhlaoxmyx
wadlgtpczgvcaqqv
inzrszbtossflsxk
dbzbtashaartczrj
qbjiqpccefcfkvod
hluujmokjywotvzy
thwlliksfztcmwzh
arahybspdaqdexrq
nuojrmsgyipdvwyx
hnajdwjwmzattvst
sulcgaxezkprjbgu
rjowuugwdpkjtypw
oeugzwuhnrgiaqga
wvxnyymwftfoswij
pqxklzkjpcqscvde
tuymjzknntekglqj
odteewktugcwlhln
exsptotlfecmgehc
eeswfcijtvzgrqel
vjhrkiwmunuiwqau
zhlixepkeijoemne
pavfsmwesuvebzdd
jzovbklnngfdmyws
nbajyohtzfeoiixz
ciozmhrsjzrwxvhz
gwucrxieqbaqfjuv
uayrxrltnohexawc
flmrbhwsfbcquffm
gjyabmngkitawlxc
rwwtggvaygfbovhg
xquiegaisynictjq
oudzwuhexrwwdbyy
lengxmguyrwhrebb
uklxpglldbgqsjls
dbmvlfeyguydfsxq
zspdwdqcrmtmdtsc
mqfnzwbfqlauvrgc
amcrkzptgacywvhv
ndxmskrwrqysrndf
mwjyhsufeqhwisju
srlrukoaenyevykt
tnpjtpwawrxbikct
geczalxmgxejulcv
tvkcbqdhmuwcxqci
tiovluvwezwwgaox
zrjhtbgajkjqzmfo
vcrywduwsklepirs
lofequdigsszuioy
wxsdzomkjqymlzat
iabaczqtrfbmypuy
ibdlmudbajikcncr
rqcvkzsbwmavdwnv
ypxoyjelhllhbeog
fdnszbkezyjbttbg
uxnhrldastpdjkdz
xfrjbehtxnlyzcka
omjyfhbibqwgcpbv
eguucnoxaoprszmp
xfpypldgcmcllyzz
aypnmgqjxjqceelv
mgzharymejlafvgf
tzowgwsubbaigdok
ilsehjqpcjwmylxc
pfmouwntfhfnmrwk
csgokybgdqwnduwp
eaxwvxvvwbrovypz
nmluqvobbbmdiwwb
lnkminvfjjzqbmio
mjiiqzycqdhfietz
towlrzriicyraevq
obiloewdvbrsfwjo
lmeooaajlthsfltw
ichygipzpykkesrw
gfysloxmqdsfskvt
saqzntehjldvwtsx
pqddoemaufpfcaew
mjrxvbvwcreaybwe
ngfbrwfqnxqosoai
nesyewxreiqvhald
kqhqdlquywotcyfy
liliptyoqujensfi
nsahsaxvaepzneqq
zaickulfjajhctye
gxjzahtgbgbabtht
koxbuopaqhlsyhrp
jhzejdjidqqtjnwe
dekrkdvprfqpcqki
linwlombdqtdeyop
dvckqqbnigdcmwmx
yaxygbjpzkvnnebv
rlzkdkgaagmcpxah
cfzuyxivtknirqvt
obivkajhsjnrxxhn
lmjhayymgpseuynn
bbjyewkwadaipyju
lmzyhwomfypoftuu
gtzhqlgltvatxack
jfflcfaqqkrrltgq
txoummmnzfrlrmcg
ohemsbfuqqpucups
imsfvowcbieotlok
tcnsnccdszxfcyde
qkcdtkwuaquajazz
arcfnhmdjezdbqku
srnocgyqrlcvlhkb
mppbzvfmcdirbyfw
xiuarktilpldwgwd
ypufwmhrvzqmexpc
itpdnsfkwgrdujmj
cmpxnodtsswkyxkr
wayyxtjklfrmvbfp
mfaxphcnjczhbbwy
sjxhgwdnqcofbdra
pnxmujuylqccjvjm
ivamtjbvairwjqwl
deijtmzgpfxrclss
bzkqcaqagsynlaer
tycefobvxcvwaulz
ctbhnywezxkdsswf
urrxxebxrthtjvib
fpfelcigwqwdjucv
ngfcyyqpqulwcphb
rltkzsiipkpzlgpw
qfdsymzwhqqdkykc
balrhhxipoqzmihj
rnwalxgigswxomga
ghqnxeogckshphgr
lyyaentdizaumnla
exriodwfzosbeoib
speswfggibijfejk
yxmxgfhvmshqszrq
hcqhngvahzgawjga
qmhlsrfpesmeksur
eviafjejygakodla
kvcfeiqhynqadbzv
fusvyhowslfzqttg
girqmvwmcvntrwau
yuavizroykfkdekz
jmcwohvmzvowrhxf
kzimlcpavapynfue
wjudcdtrewfabppq
yqpteuxqgbmqfgxh
xdgiszbuhdognniu
jsguxfwhpftlcjoh
whakkvspssgjzxre
ggvnvjurlyhhijgm
krvbhjybnpemeptr
pqedgfojyjybfbzr
jzhcrsgmnkwwtpdo
yyscxoxwofslncmp
gzjhnxytmyntzths
iteigbnqbtpvqumi
zjevfzusnjukqpfw
xippcyhkfuounxqk
mcnhrcfonfdgpkyh
pinkcyuhjkexbmzj
lotxrswlxbxlxufs
fmqajrtoabpckbnu
wfkwsgmcffdgaqxg
qfrsiwnohoyfbidr
czfqbsbmiuyusaqs
ieknnjeecucghpoo
cevdgqnugupvmsge
gjkajcyjnxdrtuvr
udzhrargnujxiclq
zqqrhhmjwermjssg
ggdivtmgoqajydzz
wnpfsgtxowkjiivl
afbhqawjbotxnqpd
xjpkifkhfjeqifdn
oyfggzsstfhvticp
kercaetahymeawxy
khphblhcgmbupmzt
iggoqtqpvaebtiol
ofknifysuasshoya
qxuewroccsbogrbv
apsbnbkiopopytgu
zyahfroovfjlythh
bxhjwfgeuxlviydq
uvbhdtvaypasaswa
qamcjzrmesqgqdiz
hjnjyzrxntiycyel
wkcrwqwniczwdxgq
hibxlvkqakusswkx
mzjyuenepwdgrkty
tvywsoqslfsulses
jqwcwuuisrclircv
xanwaoebfrzhurct
ykriratovsvxxasf
qyebvtqqxbjuuwuo
telrvlwvriylnder
acksrrptgnhkeiaa
yemwfjhiqlzsvdxf
banrornfkcymmkcc
ytbhxvaeiigjpcgm
crepyazgxquposkn
xlqwdrytzwnxzwzv
xtrbfbwopxscftps
kwbytzukgseeyjla
qtfdvavvjogybxjg
ytbmvmrcxwfkgvzw
nbscbdskdeocnfzr
sqquwjbdxsxhcseg
ewqxhigqcgszfsuw
cvkyfcyfmubzwsee
dcoawetekigxgygd
ohgqnqhfimyuqhvi
otisopzzpvnhctte
bauieohjejamzien
ewnnopzkujbvhwce
aeyqlskpaehagdiv
pncudvivwnnqspxy
ytugesilgveokxcg
zoidxeelqdjesxpr
ducjccsuaygfchzj
smhgllqqqcjfubfc
nlbyyywergronmir
prdawpbjhrzsbsvj
nmgzhnjhlpcplmui
eflaogtjghdjmxxz
qolvpngucbkprrdc
ixywxcienveltgho
mwnpqtocagenkxut
iskrfbwxonkguywx
ouhtbvcaczqzmpua
srewprgddfgmdbao
dyufrltacelchlvu
czmzcbrkecixuwzz
dtbeojcztzauofuk
prrgoehpqhngfgmw
baolzvfrrevxsyke
zqadgxshwiarkzwh
vsackherluvurqqj
surbpxdulvcvgjbd
wqxytarcxzgxhvtx
vbcubqvejcfsgrac
zqnjfeapshjowzja
hekvbhtainkvbynx
knnugxoktxpvoxnh
knoaalcefpgtvlwm
qoakaunowmsuvkus
ypkvlzcduzlezqcb
ujhcagawtyepyogh
wsilcrxncnffaxjf
gbbycjuscquaycrk
aduojapeaqwivnly
ceafyxrakviagcjy
nntajnghicgnrlst
vdodpeherjmmvbje
wyyhrnegblwvdobn
xlfurpghkpbzhhif
xyppnjiljvirmqjo
kglzqahipnddanpi
omjateouxikwxowr
ocifnoopfglmndcx
emudcukfbadyijev
ooktviixetfddfmh
wtvrhloyjewdeycg
cgjncqykgutfjhvb
nkwvpswppeffmwad
hqbcmfhzkxmnrivg
mdskbvzguxvieilr
anjcvqpavhdloaqh
erksespdevjylenq
fadxwbmisazyegup
iyuiffjmcaahowhj
ygkdezmynmltodbv
fytneukxqkjattvh
woerxfadbfrvdcnz
iwsljvkyfastccoa
movylhjranlorofe
drdmicdaiwukemep
knfgtsmuhfcvvshg
ibstpbevqmdlhajn
tstwsswswrxlzrqs
estyydmzothggudf
jezogwvymvikszwa
izmqcwdyggibliet
nzpxbegurwnwrnca
kzkojelnvkwfublh
xqcssgozuxfqtiwi
tcdoigumjrgvczfv
ikcjyubjmylkwlwq
kqfivwystpqzvhan
bzukgvyoqewniivj
iduapzclhhyfladn
fbpyzxdfmkrtfaeg
yzsmlbnftftgwadz".

day_6_input() ->
  "toggle 461,550 through 564,900
turn off 370,39 through 425,839
turn off 464,858 through 833,915
turn off 812,389 through 865,874
turn on 599,989 through 806,993
turn on 376,415 through 768,548
turn on 606,361 through 892,600
turn off 448,208 through 645,684
toggle 50,472 through 452,788
toggle 205,417 through 703,826
toggle 533,331 through 906,873
toggle 857,493 through 989,970
turn off 631,950 through 894,975
turn off 387,19 through 720,700
turn off 511,843 through 581,945
toggle 514,557 through 662,883
turn off 269,809 through 876,847
turn off 149,517 through 716,777
turn off 994,939 through 998,988
toggle 467,662 through 555,957
turn on 952,417 through 954,845
turn on 565,226 through 944,880
turn on 214,319 through 805,722
toggle 532,276 through 636,847
toggle 619,80 through 689,507
turn on 390,706 through 884,722
toggle 17,634 through 537,766
toggle 706,440 through 834,441
toggle 318,207 through 499,530
toggle 698,185 through 830,343
toggle 566,679 through 744,716
toggle 347,482 through 959,482
toggle 39,799 through 981,872
turn on 583,543 through 846,710
turn off 367,664 through 595,872
turn on 805,439 through 964,995
toggle 209,584 through 513,802
turn off 106,497 through 266,770
turn on 975,2 through 984,623
turn off 316,684 through 369,876
turn off 30,309 through 259,554
turn off 399,680 through 861,942
toggle 227,740 through 850,829
turn on 386,603 through 552,879
turn off 703,795 through 791,963
turn off 573,803 through 996,878
turn off 993,939 through 997,951
turn on 809,221 through 869,723
turn off 38,720 through 682,751
turn off 318,732 through 720,976
toggle 88,459 through 392,654
turn off 865,654 through 911,956
toggle 264,284 through 857,956
turn off 281,776 through 610,797
toggle 492,660 through 647,910
turn off 879,703 through 925,981
turn off 772,414 through 974,518
turn on 694,41 through 755,96
turn on 452,406 through 885,881
turn off 107,905 through 497,910
turn off 647,222 through 910,532
turn on 679,40 through 845,358
turn off 144,205 through 556,362
turn on 871,804 through 962,878
turn on 545,676 through 545,929
turn off 316,716 through 413,941
toggle 488,826 through 755,971
toggle 957,832 through 976,992
toggle 857,770 through 905,964
toggle 319,198 through 787,673
turn on 832,813 through 863,844
turn on 818,296 through 818,681
turn on 71,699 through 91,960
turn off 838,578 through 967,928
toggle 440,856 through 507,942
toggle 121,970 through 151,974
toggle 391,192 through 659,751
turn on 78,210 through 681,419
turn on 324,591 through 593,939
toggle 159,366 through 249,760
turn off 617,167 through 954,601
toggle 484,607 through 733,657
turn on 587,96 through 888,819
turn off 680,984 through 941,991
turn on 800,512 through 968,691
turn off 123,588 through 853,603
turn on 1,862 through 507,912
turn on 699,839 through 973,878
turn off 848,89 through 887,893
toggle 344,353 through 462,403
turn on 780,731 through 841,760
toggle 693,973 through 847,984
toggle 989,936 through 996,958
toggle 168,475 through 206,963
turn on 742,683 through 769,845
toggle 768,116 through 987,396
turn on 190,364 through 617,526
turn off 470,266 through 530,839
toggle 122,497 through 969,645
turn off 492,432 through 827,790
turn on 505,636 through 957,820
turn on 295,476 through 698,958
toggle 63,298 through 202,396
turn on 157,315 through 412,939
turn off 69,789 through 134,837
turn off 678,335 through 896,541
toggle 140,516 through 842,668
turn off 697,585 through 712,668
toggle 507,832 through 578,949
turn on 678,279 through 886,621
toggle 449,744 through 826,910
turn off 835,354 through 921,741
toggle 924,878 through 985,952
turn on 666,503 through 922,905
turn on 947,453 through 961,587
toggle 525,190 through 795,654
turn off 62,320 through 896,362
turn on 21,458 through 972,536
turn on 446,429 through 821,970
toggle 376,423 through 805,455
toggle 494,896 through 715,937
turn on 583,270 through 667,482
turn off 183,468 through 280,548
toggle 623,289 through 750,524
turn on 836,706 through 967,768
turn on 419,569 through 912,908
turn on 428,260 through 660,433
turn off 683,627 through 916,816
turn on 447,973 through 866,980
turn on 688,607 through 938,990
turn on 245,187 through 597,405
turn off 558,843 through 841,942
turn off 325,666 through 713,834
toggle 672,606 through 814,935
turn off 161,812 through 490,954
turn on 950,362 through 985,898
turn on 143,22 through 205,821
turn on 89,762 through 607,790
toggle 234,245 through 827,303
turn on 65,599 through 764,997
turn on 232,466 through 965,695
turn on 739,122 through 975,590
turn off 206,112 through 940,558
toggle 690,365 through 988,552
turn on 907,438 through 977,691
turn off 838,809 through 944,869
turn on 222,12 through 541,832
toggle 337,66 through 669,812
turn on 732,821 through 897,912
toggle 182,862 through 638,996
turn on 955,808 through 983,847
toggle 346,227 through 841,696
turn on 983,270 through 989,756
turn off 874,849 through 876,905
turn off 7,760 through 678,795
toggle 973,977 through 995,983
turn off 911,961 through 914,976
turn on 913,557 through 952,722
turn off 607,933 through 939,999
turn on 226,604 through 517,622
turn off 3,564 through 344,842
toggle 340,578 through 428,610
turn on 248,916 through 687,925
toggle 650,185 through 955,965
toggle 831,359 through 933,536
turn off 544,614 through 896,953
toggle 648,939 through 975,997
turn on 464,269 through 710,521
turn off 643,149 through 791,320
turn off 875,549 through 972,643
turn off 953,969 through 971,972
turn off 236,474 through 772,591
toggle 313,212 through 489,723
toggle 896,829 through 897,837
toggle 544,449 through 995,905
turn off 278,645 through 977,876
turn off 887,947 through 946,977
turn on 342,861 through 725,935
turn on 636,316 through 692,513
toggle 857,470 through 950,528
turn off 736,196 through 826,889
turn on 17,878 through 850,987
turn on 142,968 through 169,987
turn on 46,470 through 912,853
turn on 182,252 through 279,941
toggle 261,143 through 969,657
turn off 69,600 through 518,710
turn on 372,379 through 779,386
toggle 867,391 through 911,601
turn off 174,287 through 900,536
toggle 951,842 through 993,963
turn off 626,733 through 985,827
toggle 622,70 through 666,291
turn off 980,671 through 985,835
turn off 477,63 through 910,72
turn off 779,39 through 940,142
turn on 986,570 through 997,638
toggle 842,805 through 943,985
turn off 890,886 through 976,927
turn off 893,172 through 897,619
turn off 198,780 through 835,826
toggle 202,209 through 219,291
turn off 193,52 through 833,283
toggle 414,427 through 987,972
turn on 375,231 through 668,236
turn off 646,598 through 869,663
toggle 271,462 through 414,650
turn off 679,121 through 845,467
toggle 76,847 through 504,904
turn off 15,617 through 509,810
toggle 248,105 through 312,451
turn off 126,546 through 922,879
turn on 531,831 through 903,872
toggle 602,431 through 892,792
turn off 795,223 through 892,623
toggle 167,721 through 533,929
toggle 813,251 through 998,484
toggle 64,640 through 752,942
turn on 155,955 through 892,985
turn on 251,329 through 996,497
turn off 341,716 through 462,994
toggle 760,127 through 829,189
turn on 86,413 through 408,518
toggle 340,102 through 918,558
turn off 441,642 through 751,889
turn on 785,292 through 845,325
turn off 123,389 through 725,828
turn on 905,73 through 983,270
turn off 807,86 through 879,276
toggle 500,866 through 864,916
turn on 809,366 through 828,534
toggle 219,356 through 720,617
turn off 320,964 through 769,990
turn off 903,167 through 936,631
toggle 300,137 through 333,693
toggle 5,675 through 755,848
turn off 852,235 through 946,783
toggle 355,556 through 941,664
turn on 810,830 through 867,891
turn off 509,869 through 667,903
toggle 769,400 through 873,892
turn on 553,614 through 810,729
turn on 179,873 through 589,962
turn off 466,866 through 768,926
toggle 143,943 through 465,984
toggle 182,380 through 569,552
turn off 735,808 through 917,910
turn on 731,802 through 910,847
turn off 522,74 through 731,485
turn on 444,127 through 566,996
turn off 232,962 through 893,979
turn off 231,492 through 790,976
turn on 874,567 through 943,684
toggle 911,840 through 990,932
toggle 547,895 through 667,935
turn off 93,294 through 648,636
turn off 190,902 through 532,970
turn off 451,530 through 704,613
toggle 936,774 through 937,775
turn off 116,843 through 533,934
turn on 950,906 through 986,993
turn on 910,51 through 945,989
turn on 986,498 through 994,945
turn off 125,324 through 433,704
turn off 60,313 through 75,728
turn on 899,494 through 940,947
toggle 832,316 through 971,817
toggle 994,983 through 998,984
toggle 23,353 through 917,845
toggle 174,799 through 658,859
turn off 490,878 through 534,887
turn off 623,963 through 917,975
toggle 721,333 through 816,975
toggle 589,687 through 890,921
turn on 936,388 through 948,560
turn off 485,17 through 655,610
turn on 435,158 through 689,495
turn on 192,934 through 734,936
turn off 299,723 through 622,847
toggle 484,160 through 812,942
turn off 245,754 through 818,851
turn on 298,419 through 824,634
toggle 868,687 through 969,760
toggle 131,250 through 685,426
turn off 201,954 through 997,983
turn on 353,910 through 832,961
turn off 518,781 through 645,875
turn off 866,97 through 924,784
toggle 836,599 through 857,767
turn on 80,957 through 776,968
toggle 277,130 through 513,244
turn off 62,266 through 854,434
turn on 792,764 through 872,842
turn off 160,949 through 273,989
turn off 664,203 through 694,754
toggle 491,615 through 998,836
turn off 210,146 through 221,482
turn off 209,780 through 572,894
turn on 766,112 through 792,868
turn on 222,12 through 856,241".

day_7_input() ->
  "af AND ah -> ai
NOT lk -> ll
hz RSHIFT 1 -> is
NOT go -> gp
du OR dt -> dv
x RSHIFT 5 -> aa
at OR az -> ba
eo LSHIFT 15 -> es
ci OR ct -> cu
b RSHIFT 5 -> f
fm OR fn -> fo
NOT ag -> ah
v OR w -> x
g AND i -> j
an LSHIFT 15 -> ar
1 AND cx -> cy
jq AND jw -> jy
iu RSHIFT 5 -> ix
gl AND gm -> go
NOT bw -> bx
jp RSHIFT 3 -> jr
hg AND hh -> hj
bv AND bx -> by
er OR es -> et
kl OR kr -> ks
et RSHIFT 1 -> fm
e AND f -> h
u LSHIFT 1 -> ao
he RSHIFT 1 -> hx
eg AND ei -> ej
bo AND bu -> bw
dz OR ef -> eg
dy RSHIFT 3 -> ea
gl OR gm -> gn
da LSHIFT 1 -> du
au OR av -> aw
gj OR gu -> gv
eu OR fa -> fb
lg OR lm -> ln
e OR f -> g
NOT dm -> dn
NOT l -> m
aq OR ar -> as
gj RSHIFT 5 -> gm
hm AND ho -> hp
ge LSHIFT 15 -> gi
jp RSHIFT 1 -> ki
hg OR hh -> hi
lc LSHIFT 1 -> lw
km OR kn -> ko
eq LSHIFT 1 -> fk
1 AND am -> an
gj RSHIFT 1 -> hc
aj AND al -> am
gj AND gu -> gw
ko AND kq -> kr
ha OR gz -> hb
bn OR by -> bz
iv OR jb -> jc
NOT ac -> ad
bo OR bu -> bv
d AND j -> l
bk LSHIFT 1 -> ce
de OR dk -> dl
dd RSHIFT 1 -> dw
hz AND ik -> im
NOT jd -> je
fo RSHIFT 2 -> fp
hb LSHIFT 1 -> hv
lf RSHIFT 2 -> lg
gj RSHIFT 3 -> gl
ki OR kj -> kk
NOT ak -> al
ld OR le -> lf
ci RSHIFT 3 -> ck
1 AND cc -> cd
NOT kx -> ky
fp OR fv -> fw
ev AND ew -> ey
dt LSHIFT 15 -> dx
NOT ax -> ay
bp AND bq -> bs
NOT ii -> ij
ci AND ct -> cv
iq OR ip -> ir
x RSHIFT 2 -> y
fq OR fr -> fs
bn RSHIFT 5 -> bq
0 -> c
14146 -> b
d OR j -> k
z OR aa -> ab
gf OR ge -> gg
df OR dg -> dh
NOT hj -> hk
NOT di -> dj
fj LSHIFT 15 -> fn
lf RSHIFT 1 -> ly
b AND n -> p
jq OR jw -> jx
gn AND gp -> gq
x RSHIFT 1 -> aq
ex AND ez -> fa
NOT fc -> fd
bj OR bi -> bk
as RSHIFT 5 -> av
hu LSHIFT 15 -> hy
NOT gs -> gt
fs AND fu -> fv
dh AND dj -> dk
bz AND cb -> cc
dy RSHIFT 1 -> er
hc OR hd -> he
fo OR fz -> ga
t OR s -> u
b RSHIFT 2 -> d
NOT jy -> jz
hz RSHIFT 2 -> ia
kk AND kv -> kx
ga AND gc -> gd
fl LSHIFT 1 -> gf
bn AND by -> ca
NOT hr -> hs
NOT bs -> bt
lf RSHIFT 3 -> lh
au AND av -> ax
1 AND gd -> ge
jr OR js -> jt
fw AND fy -> fz
NOT iz -> ja
c LSHIFT 1 -> t
dy RSHIFT 5 -> eb
bp OR bq -> br
NOT h -> i
1 AND ds -> dt
ab AND ad -> ae
ap LSHIFT 1 -> bj
br AND bt -> bu
NOT ca -> cb
NOT el -> em
s LSHIFT 15 -> w
gk OR gq -> gr
ff AND fh -> fi
kf LSHIFT 15 -> kj
fp AND fv -> fx
lh OR li -> lj
bn RSHIFT 3 -> bp
jp OR ka -> kb
lw OR lv -> lx
iy AND ja -> jb
dy OR ej -> ek
1 AND bh -> bi
NOT kt -> ku
ao OR an -> ap
ia AND ig -> ii
NOT ey -> ez
bn RSHIFT 1 -> cg
fk OR fj -> fl
ce OR cd -> cf
eu AND fa -> fc
kg OR kf -> kh
jr AND js -> ju
iu RSHIFT 3 -> iw
df AND dg -> di
dl AND dn -> do
la LSHIFT 15 -> le
fo RSHIFT 1 -> gh
NOT gw -> gx
NOT gb -> gc
ir LSHIFT 1 -> jl
x AND ai -> ak
he RSHIFT 5 -> hh
1 AND lu -> lv
NOT ft -> fu
gh OR gi -> gj
lf RSHIFT 5 -> li
x RSHIFT 3 -> z
b RSHIFT 3 -> e
he RSHIFT 2 -> hf
NOT fx -> fy
jt AND jv -> jw
hx OR hy -> hz
jp AND ka -> kc
fb AND fd -> fe
hz OR ik -> il
ci RSHIFT 1 -> db
fo AND fz -> gb
fq AND fr -> ft
gj RSHIFT 2 -> gk
cg OR ch -> ci
cd LSHIFT 15 -> ch
jm LSHIFT 1 -> kg
ih AND ij -> ik
fo RSHIFT 3 -> fq
fo RSHIFT 5 -> fr
1 AND fi -> fj
1 AND kz -> la
iu AND jf -> jh
cq AND cs -> ct
dv LSHIFT 1 -> ep
hf OR hl -> hm
km AND kn -> kp
de AND dk -> dm
dd RSHIFT 5 -> dg
NOT lo -> lp
NOT ju -> jv
NOT fg -> fh
cm AND co -> cp
ea AND eb -> ed
dd RSHIFT 3 -> df
gr AND gt -> gu
ep OR eo -> eq
cj AND cp -> cr
lf OR lq -> lr
gg LSHIFT 1 -> ha
et RSHIFT 2 -> eu
NOT jh -> ji
ek AND em -> en
jk LSHIFT 15 -> jo
ia OR ig -> ih
gv AND gx -> gy
et AND fe -> fg
lh AND li -> lk
1 AND io -> ip
kb AND kd -> ke
kk RSHIFT 5 -> kn
id AND if -> ig
NOT ls -> lt
dw OR dx -> dy
dd AND do -> dq
lf AND lq -> ls
NOT kc -> kd
dy AND ej -> el
1 AND ke -> kf
et OR fe -> ff
hz RSHIFT 5 -> ic
dd OR do -> dp
cj OR cp -> cq
NOT dq -> dr
kk RSHIFT 1 -> ld
jg AND ji -> jj
he OR hp -> hq
hi AND hk -> hl
dp AND dr -> ds
dz AND ef -> eh
hz RSHIFT 3 -> ib
db OR dc -> dd
hw LSHIFT 1 -> iq
he AND hp -> hr
NOT cr -> cs
lg AND lm -> lo
hv OR hu -> hw
il AND in -> io
NOT eh -> ei
gz LSHIFT 15 -> hd
gk AND gq -> gs
1 AND en -> eo
NOT kp -> kq
et RSHIFT 5 -> ew
lj AND ll -> lm
he RSHIFT 3 -> hg
et RSHIFT 3 -> ev
as AND bd -> bf
cu AND cw -> cx
jx AND jz -> ka
b OR n -> o
be AND bg -> bh
1 AND ht -> hu
1 AND gy -> gz
NOT hn -> ho
ck OR cl -> cm
ec AND ee -> ef
lv LSHIFT 15 -> lz
ks AND ku -> kv
NOT ie -> if
hf AND hl -> hn
1 AND r -> s
ib AND ic -> ie
hq AND hs -> ht
y AND ae -> ag
NOT ed -> ee
bi LSHIFT 15 -> bm
dy RSHIFT 2 -> dz
ci RSHIFT 2 -> cj
NOT bf -> bg
NOT im -> in
ev OR ew -> ex
ib OR ic -> id
bn RSHIFT 2 -> bo
dd RSHIFT 2 -> de
bl OR bm -> bn
as RSHIFT 1 -> bl
ea OR eb -> ec
ln AND lp -> lq
kk RSHIFT 3 -> km
is OR it -> iu
iu RSHIFT 2 -> iv
as OR bd -> be
ip LSHIFT 15 -> it
iw OR ix -> iy
kk RSHIFT 2 -> kl
NOT bb -> bc
ci RSHIFT 5 -> cl
ly OR lz -> ma
z AND aa -> ac
iu RSHIFT 1 -> jn
cy LSHIFT 15 -> dc
cf LSHIFT 1 -> cz
as RSHIFT 3 -> au
cz OR cy -> da
kw AND ky -> kz
lx -> a
iw AND ix -> iz
lr AND lt -> lu
jp RSHIFT 5 -> js
aw AND ay -> az
jc AND je -> jf
lb OR la -> lc
NOT cn -> co
kh LSHIFT 1 -> lb
1 AND jj -> jk
y OR ae -> af
ck AND cl -> cn
kk OR kv -> kw
NOT cv -> cw
kl AND kr -> kt
iu OR jf -> jg
at AND az -> bb
jp RSHIFT 2 -> jq
iv AND jb -> jd
jn OR jo -> jp
x OR ai -> aj
ba AND bc -> bd
jl OR jk -> jm
b RSHIFT 1 -> v
o AND q -> r
NOT p -> q
k AND m -> n
as RSHIFT 2 -> at".

day_8_input() ->
  ["qxfcsmh",
"ffsfyxbyuhqkpwatkjgudo",
"byc\\x9dyxuafof\\\\\\xa6uf\\\\axfozomj\\\\olh\\x6a",
"jtqvz",
"uzezxa\\\"jgbmojtwyfbfguz",
"vqsremfk\\x8fxiknektafj",
"wzntebpxnnt\\\"vqndz\\\"i\\x47vvjqo\\\"",
"higvez\\\"k\\\"riewqk",
"dlkrbhbrlfrp\\\\damiauyucwhty",
"d\\\"",
"qlz",
"ku",
"yy\\\"\\\"uoao\\\"uripabop",
"saduyrntuswlnlkuppdro\\\\sicxosted",
"tj",
"zzphopswlwdhebwkxeurvizdv",
"xfoheirjoakrpofles\\\"nfu",
"q\\xb7oh\\\"p\\xce\\\"n",
"qeendp\\\"ercwgywdjeylxcv",
"dcmem",
"\\\"i\\x13r\\\"l",
"ikso\\xdcbvqnbrjduh\\\"uqudzki\\xderwk",
"wfdsn",
"pwynglklryhtsqbno",
"hcoj\\x63iccz\\\"v\\\"ttr",
"zf\\x23\\\\hlj\\\\kkce\\\\d\\\\asy\\\"yyfestwcdxyfj",
"xs",
"m\\\"tvltapxdvtrxiy",
"bmud",
"k\\\"a",
"b\\\"oas",
"\\\"yexnjjupoqsxyqnquy\\\"uzfdvetqrc",
"vdw\\xe3olxfgujaj",
"qomcxdnd\\\"\\\\cfoe\\\"",
"fpul",
"m\\\"avamefphkpv",
"vvdnb\\\\x\\\\uhnxfw\\\"dpubfkxfmeuhnxisd",
"hey\\\\",
"ldaeigghlfey",
"eure\\\"hoy\\xa5iezjp\\\\tm",
"yygb\\\"twbj\\\\r\\\"\\x10gmxuhmp\\\"",
"weirebp\\x39mqonbtmfmd",
"ltuz\\\\hs\\\"e",
"ysvmpc",
"g\\x8amjtt\\\"megl\\\"omsaihifwa",
"yimmm",
"iiyqfalh",
"cwknlaaf",
"q\\x37feg\\xc6s\\\"xx",
"uayrgeurgyp\\\\oi",
"xhug\\\"pt\\\"axugllbdiggzhvy",
"kdaarqmsjfx\\xc3d",
"\\\"vkwla",
"d\\\"",
"tmroz\\\"bvfinxoe\\\\mum\\\"wmm",
"\\\"n\\\"bbswxne\\\\p\\\\yr\\\"qhwpdd",
"skzlkietklkqovjhvj\\xfe",
"pbg\\\\pab\\\"bubqaf\\\"obzcwxwywbs\\\\dhtq",
"xxjidvqh\\\"lx\\\\wu\\\"ij",
"daef\\x5fe\\x5b\\\\kbeeb\\x13qnydtboof",
"ogvazaqy\\\"j\\x73",
"y",
"n\\\"tibetedldy\\\\gsamm\\\"nwu",
"wldkvgdtqulwkad",
"dpmxnj",
"twybw\\\"cdvf\\\"mjdajurokbce",
"ru\\\"\\\\lasij\\\"i",
"roc\\\\vra\\\\lhrm",
"pbkt\\x60booz\\\"fjlkc",
"j\\x4dytvjwrzt",
"\\\\uiwjkniumxcs",
"cbhm\\\"nexccior\\\"v\\\"j\\\"nazxilmfp\\x47",
"qdxngevzrlgoq",
"\\\"lrzxftytpobsdfyrtdqpjbpuwmm\\x9e",
"mdag\\x0asnck\\xc2ggj\\\"slb\\\"fjy",
"wyqkhjuazdtcgkcxvjkpnjdae",
"aixfk\\xc0iom\\x21vueob",
"dkiiakyjpkffqlluhaetires",
"ysspv\\\"lysgkvnmwbbsy",
"gy\\\"ryexcjjxdm\\\"xswssgtr",
"s",
"ddxv",
"qwt\\\"\\x27puilb\\\"pslmbrsxhrz",
"qdg\\xc9e\\\\qwtknlvkol\\x54oqvmchn\\\\",
"lvo",
"b",
"fk\\\"aa\\\"\\\"yenwch\\\\\\\\on",
"srig\\x63hpwaavs\\\\\\x80qzk\\\"xa\\\"\\xe6u\\\\wr",
"yxjxuj\\\"ghyhhxfj\\\"\\xa6qvatre",
"yoktqxjxkzrklkoeroil",
"\\\"jfmik\\\"",
"smgseztzdwldikbqrh\\\"",
"jftahgctf\\\"hoqy",
"tcnhicr\\\"znpgckt\\\"ble",
"vqktnkodh\\\"lo\\\"a\\\\bkmdjqqnsqr",
"ztnirfzqq",
"s",
"xx",
"iqj\\\"y\\\\hqgzflwrdsusasekyrxbp\\\\ad",
"\\\\xzjhlaiynkioz\\\"\\\"bxepzimvgwt",
"s\\x36rbw",
"mniieztwrisvdx",
"atyfxioy\\x2b\\\\",
"irde\\x85\\x5cvbah\\\\jekw\\\"ia",
"bdmftlhkwrprmpat\\\"prfaocvp",
"w\\\\k",
"umbpausy",
"zfauhpsangy",
"p\\\"zqyw",
"wtztypyqvnnxzvlvipnq\\\"zu",
"deicgwq\\\\oqvajpbov\\\\or\\\"kgplwu",
"mbzlfgpi\\\\\\\\zqcidjpzqdzxityxa",
"lfkxvhma",
"\\xf2yduqzqr\\\"\\\\fak\\\"p\\\"n",
"mpajacfuxotonpadvng",
"anb\\\\telzvcdu\\\\a\\xf2flfq",
"lrs\\\"ebethwpmuuc\\\"\\x86ygr",
"qmvdbhtumzc\\\"ci",
"meet",
"yopg\\x0fdxdq\\\"h\\\\ugsu\\xffmolxjv",
"uhy",
"fzgidrtzycsireghazscvmwcfmw\\\\t",
"cqohkhpgvpru",
"bihyigtnvmevx\\\"xx",
"xz",
"zofomwotzuxsjk\\\"q\\\"mc\\\"js\\\"dnmalhxd",
"\\\\ktnddux\\\\fqvt\\\"ibnjntjcbn",
"ia",
"htjadnefwetyp\\xd5kbrwfycbyy",
"\\\"\\\\hkuxqddnao",
"meqqsz\\x83luecpgaem",
"cvks\\x87frvxo\\\"svqivqsdpgwhukmju",
"sgmxiai\\\\o\\\"riufxwjfigr\\xdf",
"fgywdfecqufccpcdn",
"faghjoq\\x28abxnpxj",
"zuppgzcfb\\\"dctvp\\\"elup\\\"zxkopx",
"xqs\\x45xxdqcihbwghmzoa",
"anbnlp\\\\cgcvm\\\"hc",
"xf\\\"fgrngwzys",
"nrxsjduedcy\\x24",
"\\x71sxl\\\"gj\\\"sds\\\"ulcruguz\\\\t\\\\ssvjcwhi",
"jhj\\\"msch",
"qpovolktfwyiuyicbfeeju\\x01",
"nkyxmb\\\"qyqultgt\\\"nmvzvvnxnb",
"ycsrkbstgzqb\\\"uv\\\\cisn",
"s",
"ueptjnn\\\"\\\"sh",
"lp\\\"z\\\"d\\\"mxtxiy",
"yzjtvockdnvbubqabjourf\\\"k\\\"uoxwle",
"\\x82\\\"wqm\\\"",
"\\xb5cwtuks\\x5fpgh",
"wd",
"tbvf",
"ttbmzdgn",
"vfpiyfdejyrlbgcdtwzbnm",
"uc",
"otdcmhpjagqix",
"\\\\\\xb1qso\\\"s",
"scowax",
"behpstjdh\\xccqlgnqjyz\\\"eesn",
"r\\xe1cbnjwzveoomkzlo\\\\kxlfouhm",
"jgrl",
"kzqs\\\\r",
"ctscb\\x7fthwkdyko\\\"\\x62pkf\\\"d\\xe6knmhurg",
"tc\\\"kw\\x3ftt",
"bxb\\x5ccl",
"jyrmfbphsldwpq",
"jylpvysl\\\"\\\"juducjg",
"en\\\\m\\\"kxpq\\\"wpb\\\\\\\"",
"madouht\\\"bmdwvnyqvpnawiphgac\\\"",
"vuxpk\\\"ltucrw",
"aae\\x60arr",
"ttitnne\\\"kilkrgssnr\\xfdurzh",
"oalw",
"pc\\\"\\\"gktkdykzbdpkwigucqni\\\"nxiqx",
"dbrsaj",
"bgzsowyxcbrvhtvekhsh\\\"qgd",
"kudfemvk\\\"\\\"\\\"hkbrbil\\\"chkqoa",
"zjzgj\\\\ekbhyfzufy",
"\\\\acos\\\"fqekuxqzxbmkbnn\\x1ejzwrm",
"elxahvudn\\\"txtmomotgw",
"\\x2eoxmwdhelpr\\\"cgi\\xf7pzvb",
"eapheklx",
"hfvma\\\"mietvc\\\"tszbbm\\\"czex",
"h\\\"iiockj\\\\\\xc1et",
"d\\\"rmjjftm",
"qlvhdcbqtyrhlc\\\\",
"yy\\\"rsucjtulm\\\"coryri\\\"eqjlbmk",
"tv",
"r\\\"bfuht\\\\jjgujp\\\"",
"kukxvuauamtdosngdjlkauylttaokaj",
"srgost\\\"\\\"rbkcqtlccu\\x65ohjptstrjkzy",
"yxwxl\\\\yjilwwxffrjjuazmzjs",
"dxlw\\\\fkstu\\\"hjrtiafhyuoh\\\"sewabne",
"\\x88sj\\\"v",
"rfzprz\\xec\\\"oxqclu\\\"krzefp\\\\q",
"cfmhdbjuhrcymgxpylllyvpni",
"ucrmjvmimmcq\\x88\\xd9\\\"lz",
"lujtt\\\"",
"gvbqoixn\\\"pmledpjmo\\\"flydnwkfxllf",
"dvxqlbshhmelsk\\x8big\\\"l",
"mx\\x54lma\\x8bbguxejg",
"\\x66jdati\\xeceieo",
"\\\"iyyupixei\\x54ff",
"xohzf\\\"rbxsoksxamiu",
"vlhthspeshzbppa\\x4drhqnohjop\\\"\\\"mfjd",
"f\\\"tvxxla\\\"vurian\\\"\\\"idjq\\x3aptm\\xc3olep",
"gzqz",
"kbq\\\\wogye\\\\altvi\\\\hbvmodny",
"j\\xd8",
"ofjozdhkblvndl",
"hbitoupimbawimxlxqze",
"ypeleimnme",
"xfwdrzsc\\\\oxqamawyizvi\\\\y",
"enoikppx\\xa1ixe\\\"yo\\\"gumye",
"fb",
"vzf",
"zxidr",
"cu\\x31beirsywtskq",
"lxpjbvqzztafwezd",
"\\\\jyxeuo\\x18bv",
"b\\\"vawc\\\"p\\\\\\\\giern\\\"b",
"odizunx\\\"\\\"t\\\\yicdn\\\"x\\\"sdiz",
"\\\"\\\"tebrtsi",
"ctyzsxv\\xa6pegfkwsi\\\"tgyltaakytccb",
"htxwbofchvmzbppycccliyik\\xe5a",
"ggsslefamsklezqkrd",
"rcep\\\"fnimwvvdx\\\"l",
"zyrzlqmd\\x12egvqs\\\\llqyie",
"\\x07gsqyrr\\\\rcyhyspsvn",
"butg\\\"",
"gb",
"gywkoxf\\\"jsg\\\\wtopxvumirqxlwz",
"rj\\\"ir\\\"wldwveair\\x2es\\\"dhjrdehbqnzl",
"ru\\\"elktnsbxufk\\\\ejufjfjlevt\\\\lrzd",
"\\\"widsvok",
"oy\\\"\\x81nuesvw",
"ay",
"syticfac\\x1cfjsivwlmy\\\"pumsqlqqzx",
"m",
"rjjkfh\\x78cf\\x2brgceg\\\"jmdyas\\\"\\\\xlv\\xb6p",
"tmuvo\\\"\\x3ffdqdovjmdmkgpstotojkv\\\"as",
"jd\\\\ojvynhxllfzzxvbn\\\"wrpphcvx",
"pz",
"\\\"twr",
"n\\\\hdzmxe\\\"mzjjeadlz",
"fb\\\"rprxuagvahjnri",
"rfmexmjjgh\\\\xrnmyvnatrvfruflaqjnd",
"obbbde\\\"co\\\"qr\\\"qpiwjgqahqm\\\\jjp\\\"",
"vpbq\\\"\\\"y\\\"czk\\\\b\\x52ed\\\"lnzepobp",
"syzeajzfarplydipny\\\"y\\\"\\xe8ad",
"mpyodwb",
"\\x47rakphlqqptd",
"wa\\\"oj\\\"aiy",
"a",
"ropozx",
"q\\x51nbtlwa",
"etukvgx\\\\jqxlkq",
"\\\"tp\\\"rah\\\"pg\\\"s\\\"bpdtes\\\\tkasdhqd",
"dn\\\"qqpkikadowssb\\xcah\\\"dzpsf\\\\ect\\\"jdh",
"pxunovbbrrn\\\\vullyn\\\"bno\\\"\\\"\\\"myfxlp\\\"",
"qaixyazuryvkmoulhcqaotegfj\\\\mpzm",
"bvfrbicutzbjwn\\\\oml\\\"cf\\\"d\\\"ezcpv\\\"j",
"rmbrdtneudemigdhelmb",
"aq\\\\aurmbhy",
"wujqvzw",
"gf\\\"tssmvm\\\"gm\\\"hu\\x9a\\xb7yjawsa",
"hrhqqxow\\xe2gsydtdspcfqy\\\"zw\\\\ou",
"ianwwf\\\\yko\\\\tdujhhqdi",
"xylz\\\"zpvpab",
"lwuopbeeegp",
"aoop\\x49jhhcexdmdtun",
"\\\\\\\\mouqqcsgmz",
"tltuvwhveau\\x43b\\\"ymxjlcgiymcynwt",
"gsugerumpyuhtjljbhrdyoj",
"lnjm\\xb8wg\\\"ajh",
"zmspue\\\"nfttdon\\\\b\\\"eww",
"\\\"w\\x67jwaq\\x7ernmyvs\\\\rmdsuwydsd\\\"th",
"ogtgvtlmcvgllyv",
"z\\\"fqi\\\"rvddoehrciyl",
"yustxxtot\\\"muec\\\"xvfdbzunzvveq",
"mqslw",
"txqnyvzmibqgjs\\xb6xy\\x86nfalfyx",
"kzhehlmkholov",
"plpmywcnirrjutjguosh\\\\",
"pydbnqofv\\\"dn\\\\m",
"aegqof",
"eambmxt\\\\dxagoogl\\\\zapfwwlmk",
"afbmqitxxqhddlozuxcpjxgh",
"vgts",
"bfdpqtoxzzhmzcilehnflna",
"s\\\"idpz",
"\\xcfhgly\\\"nlmztwybx\\\"ecezmsxaqw",
"aackfgndqcqiy",
"\\x22unqdlsrvgzfaohoffgxzfpir\\\"s",
"abh\\\"ydv\\\"kbpdhrerl",
"bdzpg",
"ekwgkywtmzp",
"wtoodejqmrrgslhvnk\\\"pi\\\"ldnogpth",
"njro\\x68qgbx\\xe4af\\\"\\\\suan"].

day_9_input() ->
  "Faerun to Norrath = 129
Faerun to Tristram = 58
Faerun to AlphaCentauri = 13
Faerun to Arbre = 24
Faerun to Snowdin = 60
Faerun to Tambi = 71
Faerun to Straylight = 67
Norrath to Tristram = 142
Norrath to AlphaCentauri = 15
Norrath to Arbre = 135
Norrath to Snowdin = 75
Norrath to Tambi = 82
Norrath to Straylight = 54
Tristram to AlphaCentauri = 118
Tristram to Arbre = 122
Tristram to Snowdin = 103
Tristram to Tambi = 49
Tristram to Straylight = 97
AlphaCentauri to Arbre = 116
AlphaCentauri to Snowdin = 12
AlphaCentauri to Tambi = 18
AlphaCentauri to Straylight = 91
Arbre to Snowdin = 129
Arbre to Tambi = 53
Arbre to Straylight = 40
Snowdin to Tambi = 15
Snowdin to Straylight = 99
Tambi to Straylight = 70".

day_10_input() ->
  "3113322113".

day_11_input() ->
  "vzbxkghb".

day_12_input() ->
  "[[\"green\",[{\"e\":\"green\",\"a\":77,\"d\":{\"c\":\"violet\",\"a\":\"yellow\",\"b\":\"violet\"},\"c\":\"yellow\",\"h\":\"red\",\"b\":144,\"g\":{\"a\":[\"yellow\",-48,72,87,{\"e\":\"violet\",\"c\":123,\"a\":101,\"b\":87,\"d\":\"red\",\"f\":88},{\"e\":\"red\",\"c\":2,\"a\":1,\"g\":\"blue\",\"b\":\"green\",\"d\":\"violet\",\"f\":170},\"orange\",171,162]},\"f\":\"orange\",\"i\":\"orange\"},49,[{\"c\":{\"e\":\"violet\",\"a\":-44,\"d\":115,\"c\":117,\"h\":194,\"b\":{\"e\":-17,\"a\":172,\"d\":\"green\",\"c\":197,\"h\":53,\"b\":106,\"g\":\"violet\",\"f\":-10},\"g\":\"red\",\"f\":\"orange\"},\"a\":-49,\"b\":[\"violet\",\"orange\",\"blue\"]}],\"green\"]],[\"orange\"],{\"e\":\"blue\",\"a\":[\"red\",\"yellow\"],\"d\":{\"a\":[{\"c\":{\"a\":181,\"b\":[\"orange\",-40,\"red\",\"orange\",\"yellow\",31,60,71,\"yellow\"]},\"a\":[114,-40],\"b\":\"orange\"},[\"green\",93,10,{\"c\":11,\"a\":170,\"b\":[161,-3],\"d\":-16},58,{\"e\":{\"c\":-2,\"a\":117,\"b\":\"violet\"},\"c\":[\"blue\",\"yellow\",\"red\",\"violet\",\"yellow\",123,113],\"a\":\"orange\",\"g\":19,\"b\":108,\"d\":\"red\",\"f\":\"yellow\"},{\"e\":\"green\",\"c\":\"yellow\",\"a\":{\"e\":28,\"c\":\"red\",\"a\":\"violet\",\"b\":\"red\",\"d\":\"green\"},\"g\":\"yellow\",\"b\":116,\"d\":148,\"f\":\"red\"},[15],[\"green\",\"green\",43],\"blue\"],[133],\"green\",134,\"violet\",{\"c\":\"red\",\"a\":[71,41,\"blue\"],\"b\":\"yellow\",\"d\":\"violet\"},132,[10,\"violet\",[182,\"green\",\"green\",\"orange\"],78,{\"c\":\"blue\",\"a\":[100,-36,\"blue\",\"violet\",-10,\"orange\"],\"b\":{\"e\":\"orange\",\"c\":\"blue\",\"a\":160,\"g\":\"green\",\"b\":190,\"d\":\"red\",\"f\":186}},16],[{\"c\":\"green\",\"a\":\"violet\",\"b\":20,\"d\":\"red\"},\"green\",\"blue\",{\"c\":[0,84,184,\"orange\",-34,\"blue\",\"orange\",0,\"violet\",\"violet\"],\"a\":10,\"b\":89},\"green\",182,127,-2,196]]},\"c\":-20,\"h\":[[165,[180,\"yellow\",-5,16,\"red\",[{\"e\":\"orange\",\"a\":\"orange\",\"d\":\"orange\",\"c\":\"yellow\",\"h\":\"red\",\"b\":182,\"g\":21,\"f\":\"violet\"},\"red\",69,\"violet\",10,\"red\",\"orange\"]],[160,\"blue\",{\"e\":\"yellow\",\"c\":\"violet\",\"a\":\"green\",\"g\":43,\"b\":[-40,\"yellow\",\"yellow\",118,57,\"green\",\"violet\",\"yellow\",\"violet\"],\"d\":\"yellow\",\"f\":\"blue\"}],130,{\"e\":[\"yellow\",58,\"green\",139,\"violet\",\"red\"],\"c\":\"green\",\"a\":\"green\",\"g\":{\"e\":163,\"c\":33,\"a\":15,\"b\":78,\"d\":\"green\"},\"b\":86,\"d\":\"orange\",\"f\":58},\"red\",\"red\",{\"a\":37}],\"yellow\",{\"e\":44,\"a\":{\"a\":136,\"b\":\"yellow\"},\"d\":\"yellow\",\"j\":39,\"c\":[-28,[\"violet\",{\"e\":\"red\",\"a\":150,\"d\":189,\"c\":76,\"h\":\"orange\",\"b\":\"yellow\",\"g\":164,\"f\":\"orange\"}],{\"e\":12,\"a\":\"red\",\"d\":\"yellow\",\"c\":\"yellow\",\"h\":102,\"b\":\"yellow\",\"g\":\"red\",\"f\":147},\"violet\",[40,\"red\",24,193,105,179,\"violet\",\"green\",{\"a\":\"yellow\",\"b\":\"violet\"}],39,-25,{\"a\":127},126,{\"e\":\"red\",\"c\":151,\"a\":-46,\"b\":\"green\",\"d\":\"violet\"}],\"h\":\"red\",\"b\":122,\"g\":93,\"f\":[\"violet\",{\"c\":102,\"a\":-16,\"b\":39,\"d\":176},\"red\",187,{\"e\":\"blue\",\"a\":172,\"d\":{\"a\":-5},\"c\":25,\"h\":{\"a\":\"red\"},\"b\":\"blue\",\"g\":[\"yellow\",\"red\"],\"f\":{\"e\":158,\"c\":85,\"a\":\"blue\",\"g\":\"green\",\"b\":\"violet\",\"d\":125,\"f\":93},\"i\":11},\"violet\"],\"i\":[195,{\"a\":\"blue\"},48,[44,25,\"green\",\"red\",\"violet\",172,\"orange\",49]]},[\"yellow\",[{\"e\":-4,\"a\":\"red\",\"d\":\"green\",\"c\":\"blue\",\"h\":\"green\",\"b\":\"green\",\"g\":190,\"f\":30,\"i\":-2},\"green\",\"violet\",{\"e\":92,\"c\":-11,\"a\":{\"e\":\"orange\",\"c\":0,\"a\":47,\"g\":\"blue\",\"b\":\"orange\",\"d\":\"yellow\",\"f\":-47},\"b\":40,\"d\":193},97,\"violet\",[51,168,\"violet\",{\"c\":-45,\"a\":167,\"b\":\"orange\"},\"blue\",\"orange\",64],[188,\"green\",91,-10,66,\"green\"],\"green\",\"blue\"],\"green\",-42,{\"e\":\"red\",\"c\":\"red\",\"a\":199,\"g\":84,\"b\":\"blue\",\"d\":[\"blue\",194,[\"green\",88,110,-23,\"yellow\",\"green\",15,\"violet\"]],\"f\":\"red\"}],{\"e\":\"blue\",\"c\":-33,\"a\":-34,\"b\":\"blue\",\"d\":81}],\"b\":{\"e\":\"violet\",\"c\":165,\"a\":\"violet\",\"b\":{\"a\":\"red\",\"b\":\"yellow\"},\"d\":[6,71,\"orange\",\"orange\",{\"a\":96},74,\"yellow\"]},\"g\":[\"yellow\",[\"orange\",[-27],[[[8,\"red\",\"blue\",-46,62,\"yellow\",94],[60,\"violet\",141,\"green\"],16,\"green\",\"yellow\",\"blue\",81,[93,\"red\",183,\"blue\",\"red\",30,-16,\"red\",\"red\",\"yellow\"],-21,139],{\"c\":\"blue\",\"a\":\"orange\",\"b\":\"violet\",\"d\":51}],\"blue\",\"yellow\",[\"violet\",19,\"orange\",\"yellow\",\"red\",\"violet\",\"blue\",\"violet\",{\"e\":\"yellow\",\"a\":\"violet\",\"d\":137,\"c\":\"blue\",\"h\":197,\"b\":\"orange\",\"g\":{\"e\":\"yellow\",\"c\":\"orange\",\"a\":18,\"b\":42,\"d\":183,\"f\":142},\"f\":68}],\"green\",[\"blue\",85,\"violet\"]]],\"f\":\"green\"},[92,{\"c\":-26,\"a\":{\"c\":[\"orange\",\"orange\",{\"e\":131,\"c\":192,\"a\":-38,\"g\":16,\"b\":27,\"d\":\"yellow\",\"f\":-46},120,\"orange\",-28,-18,3],\"a\":[\"red\",-15,{\"e\":56,\"c\":-15,\"a\":150,\"g\":\"blue\",\"b\":\"orange\",\"d\":\"violet\",\"f\":70},-44,{\"e\":\"green\",\"c\":53,\"a\":\"blue\",\"b\":\"blue\",\"d\":-34,\"f\":\"violet\"},\"orange\",[161,\"orange\",-19,{\"e\":27,\"c\":98,\"a\":\"violet\",\"g\":\"yellow\",\"b\":-45,\"d\":191,\"f\":\"green\"}]],\"b\":-41,\"d\":[[\"orange\",\"red\",\"yellow\",120,140],[{\"e\":-1,\"c\":\"orange\",\"a\":\"yellow\",\"b\":\"yellow\",\"d\":86},42,[78,140,\"orange\",\"green\",\"orange\",-49,159],\"yellow\",\"red\",90,{\"c\":\"green\",\"a\":62,\"b\":\"violet\"}],\"violet\",178,30,\"green\",186]},\"b\":\"red\",\"d\":\"violet\"},{\"e\":-21,\"a\":{\"e\":{\"e\":{\"e\":\"orange\",\"c\":115,\"a\":199,\"b\":-43,\"d\":\"yellow\"},\"a\":\"blue\",\"d\":\"green\",\"c\":\"blue\",\"h\":\"violet\",\"b\":\"red\",\"g\":\"violet\",\"f\":{\"a\":5}},\"a\":[{\"e\":\"green\",\"a\":[20,\"yellow\",23,100,\"orange\",142,\"red\",-3],\"d\":19,\"c\":18,\"h\":\"blue\",\"b\":\"yellow\",\"g\":\"yellow\",\"f\":{\"a\":-33}}],\"d\":\"yellow\",\"j\":\"yellow\",\"c\":[\"red\",-2,95,-1,\"green\",\"violet\",12],\"h\":\"orange\",\"b\":{\"e\":[94,-14,\"orange\",68],\"c\":[\"blue\",\"yellow\",[-12,\"yellow\",126,\"orange\",199,\"red\",133],{\"e\":-14,\"a\":40,\"d\":-48,\"c\":5,\"h\":\"orange\",\"b\":\"red\",\"g\":\"yellow\",\"f\":\"red\",\"i\":\"orange\"},\"blue\",[-35,87,84,\"yellow\"],\"red\",\"red\",86,\"yellow\"],\"a\":11,\"g\":{\"e\":-34,\"a\":\"orange\",\"d\":\"blue\",\"j\":\"blue\",\"c\":\"yellow\",\"h\":\"yellow\",\"b\":\"violet\",\"g\":42,\"f\":188,\"i\":53},\"b\":\"yellow\",\"d\":{\"e\":\"red\",\"c\":84,\"a\":\"violet\",\"b\":\"orange\",\"d\":87},\"f\":[\"blue\",\"yellow\",\"red\",{\"c\":\"orange\",\"a\":\"blue\",\"b\":61},\"violet\",20,-22,129]},\"g\":{\"e\":28,\"c\":170,\"a\":[\"green\",98,\"orange\",150,\"orange\"],\"b\":\"blue\",\"d\":\"blue\"},\"f\":\"blue\",\"i\":[{\"e\":\"green\",\"a\":\"blue\",\"d\":19,\"c\":177,\"h\":-18,\"b\":{\"e\":-19,\"a\":\"green\",\"d\":\"yellow\",\"c\":172,\"h\":\"red\",\"b\":\"red\",\"g\":\"yellow\",\"f\":\"yellow\"},\"g\":{\"c\":-36,\"a\":\"red\",\"b\":\"violet\"},\"f\":121},141,\"violet\",113,81]},\"d\":{\"e\":133,\"a\":19,\"d\":-27,\"c\":{\"c\":[\"yellow\"],\"a\":105,\"b\":{\"a\":\"orange\"},\"d\":{\"e\":\"yellow\",\"c\":\"orange\",\"a\":\"orange\",\"b\":163,\"d\":\"violet\",\"f\":\"red\"}},\"h\":\"yellow\",\"b\":161,\"g\":110,\"f\":[[137,6,[195,\"violet\",179,93,\"green\",130,\"blue\",\"yellow\"],70,\"orange\",-8,-28,\"orange\",{\"e\":59,\"a\":\"red\",\"d\":128,\"j\":88,\"c\":\"violet\",\"h\":188,\"b\":0,\"g\":0,\"f\":\"green\",\"i\":2}],86,\"green\",\"orange\",68,\"yellow\"]},\"c\":{\"e\":\"violet\",\"a\":-49,\"d\":[\"blue\",140],\"c\":-29,\"h\":[\"red\",4,-45,165,[\"yellow\",\"blue\",\"blue\"]],\"b\":\"blue\",\"g\":{\"e\":{\"e\":{\"e\":\"blue\",\"c\":168,\"a\":51,\"b\":-28,\"d\":\"orange\",\"f\":\"violet\"},\"c\":\"violet\",\"a\":\"green\",\"g\":\"blue\",\"b\":-29,\"d\":121,\"f\":69},\"a\":\"violet\",\"d\":[\"orange\",[12,192,\"green\",-17,160,\"blue\"],131,\"blue\",41,{\"e\":\"violet\",\"a\":\"green\",\"d\":\"blue\",\"c\":182,\"h\":\"red\",\"b\":10,\"g\":\"blue\",\"f\":-37,\"i\":151},\"blue\"],\"c\":{\"e\":\"orange\",\"a\":182,\"d\":155,\"j\":18,\"c\":-41,\"h\":119,\"b\":148,\"g\":\"green\",\"f\":104,\"i\":141},\"h\":16,\"b\":\"blue\",\"g\":[137],\"f\":\"green\",\"i\":-35},\"f\":[[\"green\",\"red\",19,\"yellow\",\"blue\",\"red\"],[\"red\",37,[36,\"red\",-38,183,\"violet\",-17,119,93],130,-20,77,[64,115,66,\"green\"]],-13,-23,\"green\",100,\"orange\",{\"a\":\"orange\",\"b\":\"red\"}]},\"h\":[\"red\",{\"e\":28,\"c\":{\"c\":\"green\",\"a\":149,\"b\":[\"orange\",137,\"violet\",184,\"orange\",\"green\",\"red\",20,72]},\"a\":114,\"g\":[\"blue\",{\"e\":\"yellow\",\"a\":104,\"d\":\"red\",\"c\":-17,\"h\":\"blue\",\"b\":\"violet\",\"g\":\"orange\",\"f\":\"red\",\"i\":\"red\"},181,21,\"blue\",\"orange\",111,{\"e\":39,\"a\":\"orange\",\"d\":196,\"j\":119,\"c\":143,\"h\":-42,\"b\":\"green\",\"g\":190,\"f\":-43,\"i\":37}],\"b\":\"orange\",\"d\":{\"e\":[\"green\",44,\"green\",177,\"violet\",-44,160,\"violet\",85,95],\"a\":{\"e\":138,\"c\":\"yellow\",\"a\":\"yellow\",\"b\":\"blue\",\"d\":\"green\"},\"d\":\"violet\",\"j\":\"blue\",\"c\":100,\"h\":\"blue\",\"b\":104,\"g\":-28,\"f\":189,\"i\":\"orange\"},\"f\":[121,{\"c\":110,\"a\":68,\"b\":5,\"d\":57},[108],15,\"red\",[83,\"blue\",\"green\",-16,\"yellow\"],\"green\"]},[[80],160,68,187,\"green\",\"green\",94,113,2,163],34,\"orange\",[\"yellow\",{\"e\":72,\"c\":[\"green\",11,\"green\",\"green\",\"orange\"],\"a\":{\"c\":\"violet\",\"a\":13,\"b\":66,\"d\":36},\"g\":\"blue\",\"b\":\"yellow\",\"d\":140,\"f\":145},\"red\",53,-11,\"yellow\",\"blue\",148,{\"e\":\"violet\",\"a\":\"violet\",\"d\":83,\"c\":\"yellow\",\"h\":103,\"b\":-23,\"g\":36,\"f\":[154,\"red\",62,112,35],\"i\":\"violet\"}],{\"e\":174,\"a\":-49,\"d\":58,\"j\":114,\"c\":\"violet\",\"h\":[162,\"red\",54,-8,[142,178,\"red\",26,\"violet\",71,-20,38,\"orange\"],\"orange\",\"yellow\",[49,25,\"violet\",\"green\",\"blue\",189,\"green\",\"yellow\",-25,55],\"red\"],\"b\":-13,\"g\":156,\"f\":[48,107,-15,167],\"i\":\"blue\"},[\"red\",{\"a\":\"red\",\"b\":\"red\"},\"violet\",[73,\"blue\",\"violet\",\"red\"],[\"red\",\"red\"],63,-12,108]],\"b\":{\"c\":\"violet\",\"a\":{\"a\":-39,\"b\":166},\"b\":183},\"g\":36,\"f\":[45,\"yellow\",\"blue\",\"violet\",26,7,[[156,-18,\"yellow\"],-4,-37,[129,-11,[\"yellow\",179,\"violet\",\"red\",\"yellow\",\"violet\"],\"red\",\"red\",133],17,\"green\",137,\"red\",-9,\"yellow\"],\"yellow\",[{\"e\":\"green\",\"a\":\"yellow\",\"d\":52,\"c\":127,\"h\":132,\"b\":38,\"g\":\"yellow\",\"f\":\"violet\",\"i\":\"red\"},\"yellow\",\"red\",{\"e\":{\"c\":60,\"a\":126,\"b\":88},\"a\":159,\"d\":\"red\",\"j\":70,\"c\":23,\"h\":195,\"b\":178,\"g\":\"red\",\"f\":\"yellow\",\"i\":\"blue\"},13,-37,[196,146,145,\"orange\",60,\"violet\",[\"red\",144,51,\"red\",-26,172,\"yellow\",\"red\",52,\"yellow\"],157,\"green\"],[{\"a\":-7,\"b\":\"red\"},123,{\"e\":\"yellow\",\"c\":\"orange\",\"a\":\"orange\",\"b\":40,\"d\":\"blue\"},139,\"green\",\"red\",48,{\"e\":165,\"a\":60,\"d\":83,\"c\":\"yellow\",\"h\":186,\"b\":34,\"g\":\"blue\",\"f\":178,\"i\":33},[116,\"yellow\",179,18,32]],\"red\"]]},{\"c\":[6,{\"e\":{\"c\":79,\"a\":82,\"b\":\"orange\"},\"a\":\"blue\",\"d\":[38,\"red\",37,[12,134,139,\"violet\",102,60,\"green\",82,91],\"orange\",84],\"j\":\"orange\",\"c\":[\"yellow\",\"green\",\"blue\",\"violet\",{\"e\":\"orange\",\"c\":157,\"a\":\"green\",\"b\":\"blue\",\"d\":\"violet\",\"f\":\"blue\"},64,[\"violet\",176,-7,137,\"red\",57,\"yellow\"],\"yellow\",[\"blue\",170,159],\"orange\"],\"h\":170,\"b\":[3],\"g\":\"violet\",\"f\":\"violet\",\"i\":186},{\"e\":{\"e\":\"yellow\",\"c\":\"red\",\"a\":\"blue\",\"g\":[\"violet\",104],\"b\":124,\"d\":42,\"f\":\"violet\"},\"c\":-43,\"a\":-28,\"b\":[6],\"d\":[0,97,{\"e\":-9,\"a\":\"violet\",\"d\":31,\"c\":23,\"h\":40,\"b\":76,\"g\":\"red\",\"f\":94},[\"violet\"],124,68,\"green\",37]},[84,{\"e\":\"yellow\",\"c\":18,\"a\":\"orange\",\"b\":\"blue\",\"d\":\"red\"},[\"yellow\",66],81,\"orange\",-22,-10,\"green\",139],\"red\",\"yellow\"],\"a\":25,\"b\":\"yellow\"},[{\"e\":25,\"a\":{\"e\":[\"violet\",22,103,{\"e\":193,\"c\":\"red\",\"a\":\"yellow\",\"g\":\"violet\",\"b\":\"yellow\",\"d\":-33,\"f\":29},{\"e\":-10,\"a\":77,\"d\":\"blue\",\"c\":-15,\"h\":74,\"b\":-4,\"g\":\"orange\",\"f\":153},\"yellow\",176,94,\"green\",141],\"c\":\"blue\",\"a\":146,\"b\":-26,\"d\":-7,\"f\":149},\"d\":\"green\",\"c\":[\"violet\"],\"h\":\"orange\",\"b\":[[23,\"violet\",\"blue\",\"violet\",\"violet\",-40],\"orange\",\"yellow\"],\"g\":57,\"f\":[{\"e\":141,\"a\":\"yellow\",\"d\":\"red\",\"c\":138,\"h\":118,\"b\":{\"a\":\"yellow\",\"b\":\"red\"},\"g\":133,\"f\":{\"e\":169,\"c\":\"violet\",\"a\":\"green\",\"g\":193,\"b\":\"orange\",\"d\":\"violet\",\"f\":-17},\"i\":\"yellow\"},72,\"green\",\"violet\",[106,\"red\",\"red\",\"red\",\"yellow\",180,\"orange\",{\"a\":\"green\",\"b\":\"green\"},111,\"blue\"],147],\"i\":-7},[[62,65,158,\"blue\",86,\"yellow\"],[71,[168,179,\"yellow\",\"red\",\"green\"],-7],\"violet\"],\"violet\",[\"yellow\",\"red\",\"blue\",\"orange\",78,47,{\"c\":\"orange\",\"a\":\"violet\",\"b\":152},[-37,\"yellow\"],-11,6]],\"red\",{\"e\":\"red\",\"a\":\"orange\",\"d\":{\"e\":\"red\",\"a\":10,\"d\":\"blue\",\"c\":{\"c\":\"green\",\"a\":91,\"b\":\"yellow\",\"d\":-28},\"h\":158,\"b\":[[\"orange\"]],\"g\":\"blue\",\"f\":[[137,157,50,10,\"blue\",-12,\"violet\",76,76,80],[164,46,\"orange\",-23,{\"a\":\"green\"},\"yellow\",\"green\",\"green\",\"yellow\",48],47]},\"j\":{\"e\":[\"orange\",{\"e\":191,\"c\":\"orange\",\"a\":25,\"g\":\"red\",\"b\":\"yellow\",\"d\":148,\"f\":\"orange\"},-34,\"orange\",\"orange\",-36],\"a\":[{\"e\":198,\"a\":[\"blue\",80,121,36,102],\"d\":{\"e\":\"green\",\"a\":\"blue\",\"d\":76,\"j\":\"red\",\"c\":127,\"h\":\"yellow\",\"b\":\"yellow\",\"g\":\"yellow\",\"f\":163,\"i\":\"red\"},\"c\":\"yellow\",\"h\":73,\"b\":\"red\",\"g\":\"green\",\"f\":\"red\"},\"violet\",{\"e\":133,\"a\":\"blue\",\"d\":\"green\",\"j\":3,\"c\":\"violet\",\"h\":144,\"b\":25,\"g\":\"green\",\"f\":102,\"i\":\"green\"},{\"e\":{\"a\":\"violet\",\"b\":\"green\"},\"a\":\"green\",\"d\":[86],\"c\":\"green\",\"h\":3,\"b\":{\"c\":\"orange\",\"a\":\"orange\",\"b\":\"yellow\",\"d\":193},\"g\":-34,\"f\":-35,\"i\":\"green\"}],\"d\":[{\"e\":\"violet\",\"a\":\"blue\",\"d\":{\"c\":79,\"a\":\"red\",\"b\":0,\"d\":\"violet\"},\"c\":77,\"h\":\"violet\",\"b\":\"green\",\"g\":-47,\"f\":\"green\"},-49,90],\"j\":\"blue\",\"c\":\"yellow\",\"h\":[\"violet\",\"green\",28,\"green\",97,\"orange\"],\"b\":53,\"g\":{\"e\":21,\"a\":{\"c\":42,\"a\":\"blue\",\"b\":\"red\"},\"d\":\"violet\",\"c\":142,\"h\":158,\"b\":\"blue\",\"g\":[\"orange\",197,\"blue\",\"green\",\"yellow\",-3,15,-38],\"f\":62},\"f\":{\"a\":32},\"i\":79},\"c\":{\"e\":[{\"e\":[125,\"yellow\",-43,\"orange\",\"red\"],\"c\":3,\"a\":\"orange\",\"b\":\"orange\",\"d\":{\"e\":195,\"c\":16,\"a\":\"yellow\",\"b\":94,\"d\":-20,\"f\":-13},\"f\":\"yellow\"},89,[\"orange\"],\"violet\"],\"c\":{\"e\":\"red\",\"a\":\"orange\",\"d\":5,\"j\":{\"a\":92,\"b\":142},\"c\":22,\"h\":\"blue\",\"b\":\"orange\",\"g\":{\"a\":[-13,199,\"green\",133,-41,-22,\"orange\",169],\"b\":[134,\"blue\"]},\"f\":183,\"i\":[\"green\",-24,\"violet\"]},\"a\":116,\"g\":[{\"e\":57,\"a\":[\"blue\",144,44,43,\"orange\",34,\"yellow\",126,\"red\"],\"d\":\"green\",\"c\":\"violet\",\"h\":\"orange\",\"b\":\"violet\",\"g\":\"violet\",\"f\":[196,60],\"i\":-21},[168],[\"red\",\"violet\",\"yellow\",\"green\",\"yellow\",\"green\",\"blue\",113,{\"e\":\"orange\",\"a\":\"red\",\"d\":-7,\"c\":-29,\"h\":\"orange\",\"b\":-44,\"g\":\"red\",\"f\":-32}],{\"a\":116},\"blue\"],\"b\":{\"c\":{\"e\":-41,\"a\":\"orange\",\"d\":154,\"c\":\"yellow\",\"h\":-12,\"b\":\"yellow\",\"g\":\"blue\",\"f\":\"violet\",\"i\":105},\"a\":-23,\"b\":2},\"d\":161,\"f\":\"orange\"},\"h\":[187,{\"c\":\"yellow\",\"a\":\"orange\",\"b\":\"orange\",\"d\":\"red\"},[[58,\"blue\"],[111,[\"yellow\",\"green\",\"green\",\"violet\",\"green\"],\"orange\",\"blue\",112,-45],31,\"violet\"]],\"b\":{\"e\":[[13,\"blue\",-19,\"blue\",\"yellow\",144,23,17,110],\"violet\",{\"c\":\"orange\",\"a\":\"yellow\",\"b\":73,\"d\":\"red\"},9,115,\"blue\",\"violet\",\"yellow\",\"blue\",\"green\"],\"a\":[-9,\"yellow\",\"violet\",183,\"red\",14,\"blue\",192,\"yellow\",165],\"d\":\"blue\",\"j\":{\"e\":[86],\"a\":-14,\"d\":\"yellow\",\"j\":\"violet\",\"c\":\"violet\",\"h\":{\"e\":85,\"a\":[\"red\",\"yellow\",114,111,129,37,71,\"blue\"],\"d\":\"yellow\",\"c\":43,\"h\":11,\"b\":72,\"g\":128,\"f\":\"red\"},\"b\":11,\"g\":183,\"f\":34,\"i\":187},\"c\":122,\"h\":{\"c\":[93,132,\"yellow\",\"yellow\",91],\"a\":\"green\",\"b\":\"orange\"},\"b\":[{\"e\":\"blue\",\"a\":\"green\",\"d\":\"blue\",\"j\":{\"e\":\"orange\",\"a\":\"violet\",\"d\":\"orange\",\"j\":\"yellow\",\"c\":\"yellow\",\"h\":\"orange\",\"b\":\"green\",\"g\":\"orange\",\"f\":\"green\",\"i\":\"green\"},\"c\":186,\"h\":\"yellow\",\"b\":145,\"g\":112,\"f\":\"orange\",\"i\":\"orange\"},100,139,-11,{\"e\":103,\"c\":[\"green\",\"red\",-40,90,\"violet\",\"violet\",\"yellow\"],\"a\":140,\"g\":\"red\",\"b\":\"violet\",\"d\":\"red\",\"f\":\"blue\"},{\"c\":-43,\"a\":\"orange\",\"b\":66},\"red\",[\"red\",\"orange\",[\"blue\",187],76,192,50,\"yellow\",\"violet\"]],\"g\":38,\"f\":\"blue\",\"i\":[129,[30,\"green\",157,92,181,176],{\"e\":\"violet\",\"a\":127,\"d\":172,\"j\":\"yellow\",\"c\":148,\"h\":171,\"b\":\"yellow\",\"g\":{\"e\":115,\"a\":\"red\",\"d\":48,\"c\":-12,\"h\":\"blue\",\"b\":\"orange\",\"g\":\"red\",\"f\":78},\"f\":135,\"i\":79}]},\"g\":[[173,131,\"yellow\",193,162,\"yellow\"],[-5,{\"e\":[116,102,\"orange\",\"yellow\"],\"a\":189,\"d\":136,\"c\":{\"e\":72,\"a\":\"blue\",\"d\":-13,\"j\":\"yellow\",\"c\":90,\"h\":\"violet\",\"b\":169,\"g\":\"orange\",\"f\":\"blue\",\"i\":\"blue\"},\"h\":186,\"b\":\"orange\",\"g\":\"red\",\"f\":\"orange\"},{\"e\":-8,\"a\":-37,\"d\":104,\"c\":\"violet\",\"h\":\"orange\",\"b\":-31,\"g\":25,\"f\":168,\"i\":119},\"green\",32,[[197,\"orange\",\"violet\"]],\"yellow\",{\"e\":[\"red\",-16,\"yellow\"],\"a\":\"blue\",\"d\":31,\"c\":\"yellow\",\"h\":\"red\",\"b\":\"red\",\"g\":\"violet\",\"f\":20,\"i\":\"violet\"},31,80],22,{\"e\":{\"e\":\"red\",\"a\":125,\"d\":\"yellow\",\"j\":111,\"c\":34,\"h\":193,\"b\":100,\"g\":\"orange\",\"f\":31,\"i\":15},\"c\":{\"e\":75,\"a\":13,\"d\":-29,\"c\":[\"green\"],\"h\":-46,\"b\":{\"a\":58},\"g\":100,\"f\":{\"c\":\"violet\",\"a\":\"red\",\"b\":-35},\"i\":[\"violet\",\"green\",\"orange\",\"violet\",183,0,-27,96]},\"a\":\"red\",\"b\":[95,\"orange\",\"blue\",\"green\",170,{\"e\":3,\"a\":\"blue\",\"d\":125,\"j\":-25,\"c\":10,\"h\":25,\"b\":\"blue\",\"g\":182,\"f\":141,\"i\":27},[\"violet\",7,76,-37,\"red\",59,\"yellow\",29]],\"d\":78,\"f\":88}],\"f\":[\"violet\",72],\"i\":[96,[\"green\",\"orange\",63,\"red\",83,\"yellow\"],[{\"e\":\"red\",\"a\":{\"e\":\"orange\",\"a\":\"green\",\"d\":183,\"c\":\"orange\",\"h\":\"yellow\",\"b\":146,\"g\":-1,\"f\":\"red\",\"i\":\"orange\"},\"d\":\"green\",\"j\":\"red\",\"c\":\"red\",\"h\":\"yellow\",\"b\":\"violet\",\"g\":-9,\"f\":182,\"i\":\"red\"},-49,17,\"orange\",187,-2,[178,\"red\",\"red\",131,195,[94,-26,\"blue\",\"green\",0,1,101]]],-25,14,\"violet\",{\"c\":\"blue\",\"a\":\"green\",\"b\":\"orange\"},198,-2]},9],[{\"e\":[\"green\",177,[-38],{\"e\":\"green\",\"a\":[147,\"green\",[56,93,\"violet\",\"red\"],82,{\"e\":\"blue\",\"a\":\"orange\",\"d\":\"red\",\"c\":30,\"h\":\"blue\",\"b\":10,\"g\":\"orange\",\"f\":\"orange\",\"i\":82},193],\"d\":\"violet\",\"j\":69,\"c\":\"green\",\"h\":161,\"b\":-12,\"g\":{\"e\":125,\"c\":-33,\"a\":-42,\"g\":70,\"b\":{\"c\":81,\"a\":52,\"b\":\"red\",\"d\":\"violet\"},\"d\":[\"violet\"],\"f\":39},\"f\":[\"red\",\"green\",74,158,14],\"i\":\"yellow\"},[144,88,[\"yellow\",\"violet\",-1,\"blue\",109,[53,86,-36,91,\"violet\",\"green\",59,15],171,\"blue\"]],185],\"c\":{\"e\":{\"a\":92},\"a\":67,\"d\":\"violet\",\"c\":\"blue\",\"h\":[71,\"violet\",25,154,{\"e\":16,\"a\":\"red\",\"d\":\"red\",\"j\":\"violet\",\"c\":54,\"h\":\"violet\",\"b\":160,\"g\":\"orange\",\"f\":{\"e\":-47,\"a\":\"green\",\"d\":\"blue\",\"c\":56,\"h\":175,\"b\":118,\"g\":97,\"f\":\"red\"},\"i\":\"yellow\"},{\"e\":[107,\"violet\",\"violet\",\"blue\",-4,\"blue\",\"green\",82,\"red\"],\"c\":{\"e\":\"yellow\",\"c\":\"violet\",\"a\":\"orange\",\"b\":\"blue\",\"d\":\"green\"},\"a\":172,\"b\":131,\"d\":\"green\",\"f\":43},\"red\",\"green\",[\"red\",\"green\",\"violet\",\"violet\",132,\"green\",153,195,-41,[128]]],\"b\":\"yellow\",\"g\":\"green\",\"f\":{\"c\":72,\"a\":{\"c\":\"green\",\"a\":\"violet\",\"b\":\"green\",\"d\":180},\"b\":48,\"d\":[\"blue\",70,60,\"orange\",139,183,\"red\",\"red\",{\"a\":\"red\",\"b\":123},\"yellow\"]},\"i\":[66,77,\"green\",\"violet\",25,[193,\"orange\",78,\"red\",[\"violet\",\"red\",163,37,\"yellow\"]]]},\"a\":\"red\",\"b\":175,\"d\":0,\"f\":[{\"e\":38,\"a\":\"yellow\",\"d\":\"violet\",\"c\":68,\"h\":{\"e\":\"orange\",\"c\":129,\"a\":\"blue\",\"b\":\"green\",\"d\":106,\"f\":\"orange\"},\"b\":\"red\",\"g\":\"green\",\"f\":{\"e\":91,\"c\":46,\"a\":\"blue\",\"g\":\"red\",\"b\":\"yellow\",\"d\":92,\"f\":\"yellow\"}},[\"green\",65,150,86,\"orange\"],\"green\",{\"c\":\"green\",\"a\":30,\"b\":\"yellow\"}]},[\"blue\",70,143,{\"a\":\"green\",\"b\":[{\"e\":83,\"c\":63,\"a\":-2,\"g\":{\"e\":\"green\",\"c\":\"orange\",\"a\":-46,\"b\":\"yellow\",\"d\":\"red\"},\"b\":39,\"d\":\"red\",\"f\":123},\"orange\",57,34,{\"c\":\"yellow\",\"a\":{\"c\":\"blue\",\"a\":\"green\",\"b\":\"blue\"},\"b\":\"orange\",\"d\":{\"e\":\"blue\",\"a\":158,\"d\":\"red\",\"c\":69,\"h\":122,\"b\":6,\"g\":93,\"f\":\"yellow\",\"i\":163}},{\"e\":183,\"c\":99,\"a\":\"orange\",\"g\":76,\"b\":42,\"d\":31,\"f\":118}]},{\"e\":31,\"c\":[\"orange\",186,58,{\"e\":\"violet\",\"c\":9,\"a\":115,\"b\":[115,\"yellow\",19,\"violet\",\"blue\",\"yellow\"],\"d\":106}],\"a\":{\"e\":\"red\",\"c\":{\"a\":82,\"b\":180},\"a\":71,\"b\":\"yellow\",\"d\":100},\"g\":{\"c\":68,\"a\":\"red\",\"b\":{\"a\":\"blue\",\"b\":70}},\"b\":\"yellow\",\"d\":\"violet\",\"f\":-4},\"yellow\",[{\"a\":\"yellow\"},[{\"e\":\"violet\",\"a\":159,\"d\":\"violet\",\"c\":\"blue\",\"h\":{\"a\":195,\"b\":-16},\"b\":97,\"g\":74,\"f\":126,\"i\":83},-49,\"orange\",\"orange\",20,{\"e\":-37,\"c\":82,\"a\":\"blue\",\"b\":\"yellow\",\"d\":\"orange\"},\"violet\",\"green\",5],\"blue\",{\"a\":-2},{\"e\":-39,\"c\":\"yellow\",\"a\":-3,\"b\":127,\"d\":[196]},{\"c\":\"red\",\"a\":-1,\"b\":\"orange\",\"d\":166},{\"e\":{\"e\":\"red\",\"a\":97,\"d\":\"orange\",\"j\":47,\"c\":84,\"h\":-36,\"b\":-5,\"g\":\"red\",\"f\":\"yellow\",\"i\":113},\"c\":55,\"a\":{\"e\":[13,108,137,\"green\",\"green\",-9,71,-36,\"orange\",\"blue\"],\"a\":\"violet\",\"d\":95,\"c\":6,\"h\":125,\"b\":\"orange\",\"g\":\"orange\",\"f\":130},\"b\":-28,\"d\":[97,46,[-7,\"violet\",146,155,166,\"orange\",\"orange\",\"yellow\",148,\"red\"],\"orange\",40,\"red\"]}],[{\"c\":\"violet\",\"a\":\"yellow\",\"b\":64,\"d\":\"orange\"},{\"e\":{\"e\":\"violet\",\"c\":\"blue\",\"a\":{\"e\":\"yellow\",\"a\":-41,\"d\":181,\"c\":101,\"h\":\"orange\",\"b\":\"orange\",\"g\":\"blue\",\"f\":51},\"g\":-25,\"b\":\"red\",\"d\":41,\"f\":1},\"a\":25,\"d\":{\"e\":\"orange\",\"a\":\"green\",\"d\":-9,\"c\":\"orange\",\"h\":71,\"b\":\"red\",\"g\":137,\"f\":133},\"j\":[[\"yellow\",116],93,\"orange\",\"violet\",\"blue\",150,34],\"c\":66,\"h\":\"violet\",\"b\":-49,\"g\":[60,194,[136,-37,160,\"red\",\"orange\",\"red\",179,\"red\"]],\"f\":[-24,\"violet\",35],\"i\":\"blue\"},{\"e\":92,\"c\":\"blue\",\"a\":\"red\",\"b\":\"blue\",\"d\":\"green\"},\"red\",[126],96,\"red\",198],87],{\"e\":{\"e\":[\"orange\",\"violet\",{\"e\":\"green\",\"a\":-42,\"d\":103,\"c\":[\"violet\",-48,37,122,107,\"orange\",\"blue\",97],\"h\":\"blue\",\"b\":92,\"g\":\"orange\",\"f\":0},\"blue\",197,-9,\"yellow\",{\"a\":[\"orange\",\"blue\",186,\"blue\",\"green\",\"red\",\"red\",48,\"red\",\"green\"],\"b\":195},121,\"blue\"],\"a\":96,\"d\":\"orange\",\"j\":94,\"c\":66,\"h\":{\"a\":[\"violet\"],\"b\":\"orange\"},\"b\":\"violet\",\"g\":191,\"f\":{\"e\":\"red\",\"c\":-32,\"a\":[149,[69,\"green\",84,25,\"red\"],\"yellow\",\"violet\",4,\"violet\",\"green\",69],\"b\":\"blue\",\"d\":148,\"f\":111},\"i\":93},\"a\":[181],\"d\":{\"e\":{\"e\":{\"e\":\"red\",\"c\":36,\"a\":143,\"b\":82,\"d\":11},\"a\":168,\"d\":\"orange\",\"j\":-45,\"c\":159,\"h\":\"red\",\"b\":{\"e\":120,\"a\":-37,\"d\":\"green\",\"c\":\"green\",\"h\":\"red\",\"b\":59,\"g\":\"violet\",\"f\":173},\"g\":166,\"f\":\"orange\",\"i\":\"yellow\"},\"a\":158,\"d\":\"green\",\"c\":126,\"h\":[[159,\"violet\",\"violet\",\"green\",101,\"orange\",141],\"violet\",122,\"yellow\",\"red\",79],\"b\":13,\"g\":\"red\",\"f\":{\"a\":\"orange\"},\"i\":{\"a\":89,\"b\":{\"e\":[-20,\"green\",6,58,18],\"a\":-17,\"d\":137,\"c\":[-25,\"orange\",95,\"yellow\",\"green\"],\"h\":3,\"b\":\"violet\",\"g\":26,\"f\":\"green\",\"i\":168}}},\"c\":[\"orange\",{\"e\":163,\"a\":{\"a\":6},\"d\":-25,\"c\":164,\"h\":[-47,\"yellow\",\"orange\",[139,93,93,\"yellow\",\"violet\",\"red\",-12],\"blue\",-32,136,10],\"b\":\"orange\",\"g\":\"blue\",\"f\":174}],\"h\":[\"blue\",-34,-29,{\"e\":\"violet\",\"a\":3,\"d\":\"green\",\"j\":\"red\",\"c\":\"orange\",\"h\":\"green\",\"b\":\"red\",\"g\":\"green\",\"f\":124,\"i\":{\"e\":186,\"c\":\"violet\",\"a\":168,\"g\":110,\"b\":[127,136,31,109,\"blue\",\"red\",\"blue\",\"violet\",79,91],\"d\":\"red\",\"f\":[\"violet\",191,-15,-22]}}],\"b\":[124,{\"e\":-38,\"a\":{\"a\":\"yellow\"},\"d\":[130,{\"c\":158,\"a\":\"blue\",\"b\":103,\"d\":197},-36,[153,-6,173,121,\"yellow\",94,168,\"violet\",77,-35],168,\"red\",{\"e\":-32,\"a\":\"red\",\"d\":46,\"c\":82,\"h\":91,\"b\":\"blue\",\"g\":\"yellow\",\"f\":\"orange\",\"i\":174},\"green\"],\"j\":[189,-43,41],\"c\":[185],\"h\":[182],\"b\":[139,\"violet\",-44],\"g\":\"yellow\",\"f\":\"red\",\"i\":[\"red\",-18,\"violet\",\"red\",31,\"red\",115,-49,[\"yellow\",\"yellow\",\"violet\",\"blue\",\"violet\",\"violet\"]]},143,\"yellow\",[\"violet\",\"red\",[\"blue\",\"violet\",{\"a\":-49},41,\"orange\",\"blue\"],{\"a\":\"orange\"},[93,-8,\"yellow\",-39]],166,155,\"red\",\"violet\",\"orange\"],\"g\":\"violet\",\"f\":\"red\",\"i\":[85,126,{\"e\":{\"e\":\"red\",\"c\":-42,\"a\":51,\"b\":\"yellow\",\"d\":\"red\",\"f\":{\"e\":130,\"c\":\"violet\",\"a\":115,\"g\":\"violet\",\"b\":-28,\"d\":-3,\"f\":\"blue\"}},\"a\":92,\"d\":114,\"c\":\"violet\",\"h\":{\"e\":-41,\"a\":\"red\",\"d\":57,\"j\":82,\"c\":\"violet\",\"h\":\"green\",\"b\":\"red\",\"g\":2,\"f\":-20,\"i\":78},\"b\":\"yellow\",\"g\":\"violet\",\"f\":86,\"i\":67},147,146,-33,\"blue\",\"violet\"]},{\"e\":[[{\"a\":174},21,\"orange\",\"green\",\"blue\",{\"e\":127,\"c\":{\"c\":0,\"a\":197,\"b\":\"yellow\"},\"a\":\"blue\",\"b\":[\"yellow\",153,9,\"blue\"],\"d\":136}],\"blue\"],\"c\":[{\"c\":\"red\",\"a\":[-5,\"green\",[\"violet\"],-47,19,173],\"b\":106,\"d\":\"yellow\"},182,[21,106,\"violet\",10,\"green\",20,\"orange\"],[\"green\"],{\"e\":-25,\"c\":\"blue\",\"a\":\"violet\",\"b\":[\"red\",27,\"blue\",21,193,\"green\",[\"green\",\"green\"]],\"d\":[\"orange\"],\"f\":18},\"yellow\",\"yellow\",{\"e\":{\"e\":\"violet\",\"a\":\"green\",\"d\":\"violet\",\"c\":\"red\",\"h\":171,\"b\":[\"red\",149,\"violet\"],\"g\":\"yellow\",\"f\":\"blue\",\"i\":\"green\"},\"c\":[86,-30,\"orange\",56,123,\"green\"],\"a\":65,\"b\":[86,129,\"yellow\",\"blue\",87,127,182],\"d\":4,\"f\":[-36,179,\"red\",-9,27,{\"c\":111,\"a\":178,\"b\":\"yellow\",\"d\":25},\"red\",\"blue\"]},[-3,5,[\"orange\",\"blue\"],70],\"yellow\"],\"a\":[{\"e\":{\"c\":-15,\"a\":\"red\",\"b\":-18,\"d\":\"green\"},\"a\":-42,\"d\":{\"c\":-47,\"a\":\"red\",\"b\":\"green\",\"d\":\"yellow\"},\"c\":{\"e\":56,\"a\":\"green\",\"d\":\"yellow\",\"c\":\"orange\",\"h\":\"yellow\",\"b\":\"blue\",\"g\":-35,\"f\":179,\"i\":\"green\"},\"h\":\"blue\",\"b\":[35,153],\"g\":193,\"f\":{\"e\":{\"e\":37,\"c\":86,\"a\":\"green\",\"g\":170,\"b\":\"violet\",\"d\":\"red\",\"f\":-33},\"c\":187,\"a\":16,\"b\":147,\"d\":19,\"f\":\"red\"},\"i\":88},\"green\",\"blue\",{\"e\":{\"e\":\"orange\",\"a\":\"blue\",\"d\":\"orange\",\"c\":150,\"h\":-12,\"b\":\"green\",\"g\":\"red\",\"f\":145,\"i\":\"red\"},\"c\":\"red\",\"a\":\"yellow\",\"b\":\"yellow\",\"d\":\"blue\"},196,{\"e\":\"green\",\"c\":186,\"a\":\"green\",\"g\":-18,\"b\":\"red\",\"d\":[102,\"green\",\"orange\",[-6,160,128,\"green\",\"violet\",48,\"violet\",\"yellow\",50],\"blue\",\"green\",\"orange\",[199,59,20,15,126]],\"f\":[[120,\"red\",69],49,18,84,\"red\",\"green\",[\"orange\",\"blue\",-31,\"green\",\"red\",198,115]]},{\"e\":\"yellow\",\"a\":196,\"d\":[\"orange\",{\"e\":\"violet\",\"a\":21,\"d\":\"green\",\"c\":\"red\",\"h\":\"green\",\"b\":18,\"g\":48,\"f\":174,\"i\":\"orange\"},{\"a\":-1,\"b\":\"green\"},\"green\"],\"c\":106,\"h\":\"blue\",\"b\":\"blue\",\"g\":\"yellow\",\"f\":{\"e\":-4,\"a\":61,\"d\":18,\"c\":122,\"h\":\"green\",\"b\":84,\"g\":165,\"f\":\"orange\"}}],\"b\":[43,{\"e\":137,\"c\":\"green\",\"a\":\"green\",\"b\":75,\"d\":125}],\"d\":{\"e\":178,\"c\":[-21,[116,20,\"yellow\",\"blue\",161,\"orange\",\"blue\",30,{\"c\":181,\"a\":-30,\"b\":3}],\"orange\",-9,\"orange\",[\"violet\",\"green\",54],\"orange\",[-20,97,{\"c\":59,\"a\":115,\"b\":-48,\"d\":-22},28,{\"e\":59,\"c\":\"green\",\"a\":\"green\",\"b\":\"yellow\",\"d\":\"green\",\"f\":-27}],{\"a\":\"violet\"},{\"e\":\"blue\",\"c\":50,\"a\":\"orange\",\"b\":\"yellow\",\"d\":\"orange\",\"f\":{\"a\":\"red\",\"b\":\"green\"}}],\"a\":\"orange\",\"b\":134,\"d\":-3,\"f\":{\"e\":\"violet\",\"a\":\"orange\",\"d\":\"green\",\"c\":80,\"h\":\"red\",\"b\":[140],\"g\":\"red\",\"f\":\"red\"}}},-47,[-28,{\"a\":[46,[\"blue\",-45,172,193,\"blue\",\"green\",-2],122,{\"a\":\"green\",\"b\":92},-35,[136,[-8,127,20,91,45,\"orange\"],\"green\",\"orange\",[\"orange\",\"yellow\",92,162,48,\"orange\",\"violet\",197],\"blue\",\"orange\",57,172],\"green\",135],\"b\":{\"e\":[173,{\"e\":89,\"a\":96,\"d\":\"orange\",\"c\":\"orange\",\"h\":\"green\",\"b\":74,\"g\":\"yellow\",\"f\":60,\"i\":135},-11,3,\"blue\",\"violet\",\"blue\"],\"c\":160,\"a\":\"blue\",\"b\":60,\"d\":\"green\",\"f\":\"red\"}},{\"e\":[12,\"orange\"],\"c\":{\"e\":45,\"c\":{\"e\":-26,\"a\":86,\"d\":\"yellow\",\"c\":[\"yellow\",128,180,135,102,186,\"red\",194,\"green\"],\"h\":\"violet\",\"b\":{\"c\":90,\"a\":-47,\"b\":56},\"g\":\"blue\",\"f\":\"red\",\"i\":28},\"a\":71,\"b\":\"violet\",\"d\":25},\"a\":182,\"b\":\"green\",\"d\":111,\"f\":\"violet\"},{\"c\":[-21,{\"e\":[157,13,\"red\",180,\"yellow\",\"green\",\"red\",59],\"c\":-49,\"a\":82,\"b\":69,\"d\":{\"e\":\"orange\",\"a\":\"yellow\",\"d\":98,\"j\":60,\"c\":\"red\",\"h\":199,\"b\":172,\"g\":120,\"f\":\"yellow\",\"i\":98}},[53,[-23,\"orange\",135,102,165,170,172,\"violet\"],\"yellow\",\"blue\",\"green\",105,97],[74,\"violet\",\"orange\",[\"yellow\",56,\"orange\",81,\"violet\"],\"orange\",177,75,11],\"blue\",\"yellow\",\"blue\",\"red\",[\"blue\"]],\"a\":{\"a\":-36,\"b\":\"orange\"},\"b\":5,\"d\":\"green\"},179,-26,{\"a\":\"green\",\"b\":[163,{\"a\":\"orange\",\"b\":-35},{\"e\":180,\"a\":\"blue\",\"d\":40,\"j\":\"orange\",\"c\":\"green\",\"h\":\"orange\",\"b\":\"orange\",\"g\":131,\"f\":53,\"i\":169}]}]],[[{\"e\":153,\"c\":\"orange\",\"a\":\"yellow\",\"g\":{\"e\":-4,\"c\":{\"e\":115,\"c\":\"red\",\"a\":121,\"b\":151,\"d\":\"red\"},\"a\":\"orange\",\"b\":194,\"d\":\"orange\"},\"b\":{\"a\":30},\"d\":[\"green\",[31,[\"violet\"],[\"orange\",152,\"yellow\",\"red\"],\"yellow\"],\"violet\",{\"a\":\"violet\"},171,\"violet\",{\"c\":\"violet\",\"a\":\"orange\",\"b\":\"orange\"},\"green\"],\"f\":{\"e\":\"yellow\",\"c\":[\"green\",\"red\"],\"a\":\"red\",\"g\":-17,\"b\":-3,\"d\":-42,\"f\":150}},\"orange\",{\"c\":[\"orange\"],\"a\":\"red\",\"b\":{\"c\":-32,\"a\":{\"a\":\"blue\",\"b\":\"orange\"},\"b\":{\"c\":75,\"a\":{\"e\":\"green\",\"a\":101,\"d\":-3,\"j\":\"violet\",\"c\":56,\"h\":166,\"b\":192,\"g\":-5,\"f\":-22,\"i\":100},\"b\":\"yellow\"},\"d\":159},\"d\":{\"e\":\"violet\",\"a\":\"yellow\",\"d\":[\"green\",\"orange\",\"blue\",\"green\"],\"j\":\"yellow\",\"c\":[23,\"green\",\"blue\",\"yellow\",\"violet\",\"red\"],\"h\":[149,-24,\"red\",152],\"b\":-12,\"g\":\"red\",\"f\":89,\"i\":169}},[{\"e\":{\"e\":\"violet\",\"a\":[124,\"blue\",\"orange\",\"green\",160],\"d\":113,\"c\":\"red\",\"h\":\"blue\",\"b\":[\"violet\",\"red\",\"violet\",104],\"g\":85,\"f\":179,\"i\":{\"e\":\"orange\",\"a\":\"violet\",\"d\":\"violet\",\"j\":\"yellow\",\"c\":191,\"h\":\"red\",\"b\":53,\"g\":-25,\"f\":\"green\",\"i\":169}},\"a\":[147,120],\"d\":\"green\",\"j\":[\"yellow\",[\"yellow\",108,\"violet\",114,\"green\",195,25,\"green\"],\"green\",\"orange\"],\"c\":[\"orange\",148,141,\"yellow\",32,-24],\"h\":124,\"b\":\"orange\",\"g\":[121,\"blue\",\"red\",\"violet\",-18],\"f\":\"violet\",\"i\":\"red\"},[\"violet\",\"green\",94,91,\"blue\"],{\"c\":[9,\"violet\",-18,69,\"orange\",\"orange\",-24,\"yellow\",\"yellow\"],\"a\":\"yellow\",\"b\":150,\"d\":73}],110,[\"blue\",-9,\"blue\",\"yellow\",{\"e\":[163,45,67],\"c\":135,\"a\":50,\"b\":[43,26,18,120,\"green\",\"blue\",10,\"green\",68,-2],\"d\":{\"e\":136,\"a\":15,\"d\":89,\"c\":[65,\"green\",108,122,\"yellow\",\"violet\",\"yellow\"],\"h\":\"green\",\"b\":{\"e\":125,\"a\":21,\"d\":51,\"c\":153,\"h\":33,\"b\":158,\"g\":\"blue\",\"f\":26,\"i\":\"green\"},\"g\":-33,\"f\":[-16,14,\"red\",\"red\",126,\"violet\",-16]}},\"red\",{\"e\":120,\"c\":\"red\",\"a\":{\"e\":\"green\",\"a\":\"yellow\",\"d\":18,\"c\":150,\"h\":185,\"b\":[\"yellow\",\"red\",\"violet\",48,\"violet\",\"blue\"],\"g\":\"green\",\"f\":{\"a\":\"yellow\"}},\"g\":[12,\"blue\",168,\"orange\",{\"a\":\"red\"},168,\"red\"],\"b\":-2,\"d\":{\"e\":\"violet\",\"c\":\"yellow\",\"a\":86,\"g\":155,\"b\":3,\"d\":-24,\"f\":149},\"f\":\"yellow\"}],[{\"c\":\"green\",\"a\":91,\"b\":\"green\"},[{\"e\":\"red\",\"c\":98,\"a\":{\"c\":\"red\",\"a\":-49,\"b\":176,\"d\":105},\"b\":\"red\",\"d\":169,\"f\":\"blue\"},-46,\"green\",\"blue\",-30,{\"c\":154,\"a\":72,\"b\":\"yellow\"},[161,85]],111,\"blue\",172,57,{\"a\":-23,\"b\":\"green\"},81,[160,[\"blue\",\"green\",\"green\",157],137,[\"blue\",[\"violet\",162],[153,\"yellow\",\"orange\",\"violet\",127,\"green\",148,182,29,150],-33,168,\"blue\"],95]]],[\"yellow\",132,{\"c\":16,\"a\":[113],\"b\":\"red\",\"d\":{\"e\":\"green\",\"c\":24,\"a\":{\"a\":\"red\"},\"g\":125,\"b\":[\"violet\",2,101],\"d\":\"green\",\"f\":132}},89],[{\"e\":\"yellow\",\"c\":[68,\"green\",[160,146],175,\"orange\",185,\"blue\",[198,[179,\"yellow\",\"green\",72,33,112,179,\"violet\",194,1]]],\"a\":[\"blue\",\"violet\",153,\"blue\",{\"a\":77,\"b\":\"yellow\"},-19,\"yellow\",\"green\"],\"b\":\"yellow\",\"d\":29,\"f\":{\"c\":\"violet\",\"a\":\"violet\",\"b\":-18}},\"yellow\",{\"e\":[\"violet\",\"green\",[\"red\",\"red\",\"blue\",126,137,47,[\"blue\",\"blue\",\"green\",102,\"orange\",\"yellow\",\"green\"],\"red\"],[[\"blue\",\"orange\",16,-2],\"green\",\"orange\",\"yellow\",27,150,0],{\"e\":62,\"c\":\"yellow\",\"a\":\"blue\",\"b\":94,\"d\":10,\"f\":31},[-47,{\"e\":\"green\",\"a\":\"yellow\",\"d\":197,\"j\":99,\"c\":\"yellow\",\"h\":152,\"b\":\"orange\",\"g\":85,\"f\":\"green\",\"i\":\"green\"},174,\"blue\",\"green\"],106],\"c\":[\"blue\",118,[120,161,-41,[\"violet\"],\"violet\",\"yellow\"],\"red\"],\"a\":[\"orange\",\"green\",\"red\",58,\"green\",5,178,191,-43],\"b\":62,\"d\":\"violet\"},{\"e\":97,\"a\":[[\"yellow\",122,\"orange\",\"red\",{\"e\":165,\"c\":\"blue\",\"a\":63,\"g\":\"violet\",\"b\":\"blue\",\"d\":\"yellow\",\"f\":77},[104,118,\"green\",\"red\",\"orange\",61]],{\"c\":124,\"a\":86,\"b\":\"violet\"},\"yellow\",{\"e\":[98,110,40,104,126,90,140,\"blue\",46],\"a\":-42,\"d\":[77,\"green\",\"red\",-28,\"blue\",88,29,-9,-28],\"j\":-3,\"c\":\"green\",\"h\":{\"e\":153,\"c\":45,\"a\":\"green\",\"g\":127,\"b\":\"red\",\"d\":183,\"f\":\"orange\"},\"b\":3,\"g\":0,\"f\":[\"blue\",\"green\",62,\"red\",\"yellow\",\"green\"],\"i\":\"orange\"},127,171,[118,[119,\"blue\"],15,87,\"orange\",{\"e\":-34,\"c\":\"violet\",\"a\":18,\"b\":153,\"d\":37,\"f\":\"red\"}],\"red\"],\"d\":\"red\",\"c\":[\"yellow\",\"red\",\"blue\",\"red\",\"violet\",\"red\",-13,179],\"h\":100,\"b\":{\"e\":\"orange\",\"a\":48,\"d\":\"red\",\"c\":{\"e\":\"violet\",\"a\":\"violet\",\"d\":\"orange\",\"c\":[\"violet\",\"yellow\",\"blue\",\"yellow\"],\"h\":\"red\",\"b\":\"violet\",\"g\":149,\"f\":\"green\"},\"h\":\"yellow\",\"b\":46,\"g\":\"blue\",\"f\":184},\"g\":16,\"f\":36,\"i\":{\"e\":\"green\",\"a\":-4,\"d\":{\"c\":{\"e\":\"yellow\",\"a\":\"blue\",\"d\":177,\"j\":\"violet\",\"c\":\"red\",\"h\":140,\"b\":131,\"g\":137,\"f\":53,\"i\":28},\"a\":16,\"b\":161},\"c\":48,\"h\":[53,7,[137,80,113,160,\"blue\",105]],\"b\":\"blue\",\"g\":\"orange\",\"f\":\"green\"}},\"yellow\",{\"c\":[\"orange\",{\"e\":-11,\"c\":-36,\"a\":\"green\",\"b\":\"yellow\",\"d\":\"yellow\",\"f\":{\"a\":186}},-4,170,\"green\",\"green\",16,123],\"a\":-29,\"b\":{\"c\":\"orange\",\"a\":\"blue\",\"b\":\"orange\"}},{\"c\":[[161,\"violet\",\"blue\"],\"yellow\",\"yellow\",[\"red\",22,[\"red\",92,103,126,-13,67,\"blue\"],-21,136,\"violet\",[193]],\"blue\",-15],\"a\":{\"e\":179,\"a\":1,\"d\":\"yellow\",\"c\":[92,15],\"h\":\"orange\",\"b\":{\"e\":-6,\"a\":\"violet\",\"d\":\"yellow\",\"j\":155,\"c\":198,\"h\":-18,\"b\":14,\"g\":\"blue\",\"f\":-39,\"i\":\"orange\"},\"g\":{\"a\":\"yellow\"},\"f\":\"blue\"},\"b\":87},\"yellow\",{\"c\":\"blue\",\"a\":[{\"e\":\"red\",\"a\":\"blue\",\"d\":\"orange\",\"c\":\"orange\",\"h\":-27,\"b\":\"yellow\",\"g\":47,\"f\":{\"e\":\"violet\",\"a\":\"green\",\"d\":185,\"j\":\"orange\",\"c\":\"violet\",\"h\":138,\"b\":-3,\"g\":\"blue\",\"f\":\"red\",\"i\":150}},{\"e\":75,\"c\":168,\"a\":[12,\"blue\",\"green\"],\"g\":{\"c\":\"blue\",\"a\":\"green\",\"b\":107},\"b\":-36,\"d\":\"orange\",\"f\":72},2,[120,\"green\",182,\"yellow\",-23,\"red\"],\"green\",{\"e\":\"blue\",\"a\":\"orange\",\"d\":\"blue\",\"c\":157,\"h\":\"green\",\"b\":58,\"g\":\"blue\",\"f\":-39},\"red\",\"orange\",32],\"b\":\"violet\"},{\"e\":178,\"a\":{\"c\":{\"e\":59,\"a\":186,\"d\":\"orange\",\"c\":{\"a\":\"violet\"},\"h\":\"green\",\"b\":198,\"g\":{\"a\":\"blue\",\"b\":\"blue\"},\"f\":\"orange\",\"i\":2},\"a\":\"red\",\"b\":[95,130,\"blue\",\"violet\",98]},\"d\":176,\"c\":-38,\"h\":[\"yellow\",128,\"green\",39,74,\"yellow\",5],\"b\":\"blue\",\"g\":\"violet\",\"f\":\"orange\"}],{\"a\":{\"e\":135,\"c\":{\"e\":{\"e\":[\"orange\",-44,81,-11,-1,47,\"orange\",-36],\"c\":10,\"a\":12,\"b\":\"red\",\"d\":{\"e\":\"violet\",\"c\":\"violet\",\"a\":161,\"b\":192,\"d\":133},\"f\":77},\"c\":92,\"a\":\"yellow\",\"g\":[\"blue\"],\"b\":{\"e\":\"violet\",\"a\":-48,\"d\":\"orange\",\"c\":\"blue\",\"h\":\"orange\",\"b\":-40,\"g\":81,\"f\":77},\"d\":102,\"f\":\"yellow\"},\"a\":127,\"b\":\"violet\",\"d\":{\"e\":130,\"a\":36,\"d\":148,\"c\":\"yellow\",\"h\":117,\"b\":\"orange\",\"g\":\"orange\",\"f\":-19,\"i\":[\"green\",{\"e\":\"red\",\"a\":191,\"d\":159,\"j\":\"violet\",\"c\":\"red\",\"h\":147,\"b\":\"blue\",\"g\":\"red\",\"f\":\"red\",\"i\":\"green\"},\"blue\",62]},\"f\":-5},\"b\":[[\"green\",-36,62,\"green\",\"blue\",{\"c\":\"violet\",\"a\":{\"a\":-4},\"b\":\"violet\",\"d\":88},{\"e\":\"yellow\",\"c\":166,\"a\":[\"blue\"],\"g\":50,\"b\":146,\"d\":\"blue\",\"f\":142},\"yellow\"]]},[\"red\",\"blue\",{\"e\":{\"a\":-16},\"a\":[[\"violet\",{\"e\":\"blue\",\"a\":171,\"d\":\"blue\",\"c\":\"blue\",\"h\":\"green\",\"b\":\"green\",\"g\":\"orange\",\"f\":\"yellow\"},186,\"orange\",195,87,\"green\",[67,158,\"blue\",23]],25],\"d\":[159,74],\"c\":-28,\"h\":{\"e\":-16,\"a\":\"red\",\"d\":55,\"c\":158,\"h\":167,\"b\":\"red\",\"g\":27,\"f\":{\"e\":\"yellow\",\"c\":[34,\"blue\",-22,\"orange\"],\"a\":94,\"b\":-30,\"d\":[\"blue\",133,39,102,\"orange\"]}},\"b\":119,\"g\":{\"e\":104,\"c\":90,\"a\":[\"orange\",\"blue\",158,-34,\"violet\"],\"g\":\"green\",\"b\":33,\"d\":[\"violet\",125,\"yellow\",\"yellow\",117,[\"blue\",25,\"orange\"],[\"red\",193,-23,\"red\",\"green\",146,173],\"red\",\"yellow\",10],\"f\":50},\"f\":47,\"i\":{\"e\":[121,144,172,171,{\"e\":\"green\",\"a\":9,\"d\":\"violet\",\"c\":-33,\"h\":64,\"b\":-4,\"g\":45,\"f\":75}],\"a\":8,\"d\":{\"c\":68,\"a\":[\"yellow\",\"red\",\"green\"],\"b\":\"violet\",\"d\":9},\"c\":\"blue\",\"h\":17,\"b\":199,\"g\":115,\"f\":[[\"green\",108,113,\"red\",6,\"violet\",\"violet\",\"green\",57,\"green\"],\"orange\",177,\"red\",34,\"blue\",\"red\"],\"i\":-25}},{\"e\":\"blue\",\"a\":[\"orange\",\"yellow\",[\"violet\",27,\"violet\",128,120,{\"e\":\"green\",\"a\":\"orange\",\"d\":\"orange\",\"j\":\"blue\",\"c\":\"yellow\",\"h\":\"yellow\",\"b\":\"yellow\",\"g\":139,\"f\":132,\"i\":81},\"blue\",\"red\",53],7,\"orange\",\"violet\",{\"c\":\"yellow\",\"a\":{\"a\":-11},\"b\":\"orange\",\"d\":87},\"violet\"],\"d\":[{\"a\":175},[163,\"orange\",185],[-30,109,194,119,170,\"green\",\"violet\",\"yellow\",125,\"red\"],\"blue\",{\"e\":\"green\",\"c\":152,\"a\":37,\"b\":\"red\",\"d\":[\"orange\"],\"f\":\"orange\"},69,\"yellow\",{\"e\":{\"e\":\"yellow\",\"c\":\"violet\",\"a\":144,\"b\":\"yellow\",\"d\":141,\"f\":\"blue\"},\"c\":\"blue\",\"a\":\"green\",\"g\":\"yellow\",\"b\":178,\"d\":\"yellow\",\"f\":-8},\"green\",[\"green\",\"orange\",-42,\"orange\"]],\"c\":\"red\",\"h\":54,\"b\":\"orange\",\"g\":[\"green\",2,146,-6,{\"e\":52,\"a\":\"orange\",\"d\":\"red\",\"c\":\"yellow\",\"h\":141,\"b\":35,\"g\":{\"e\":\"violet\",\"a\":\"blue\",\"d\":\"yellow\",\"c\":\"blue\",\"h\":100,\"b\":119,\"g\":\"blue\",\"f\":\"yellow\"},\"f\":136,\"i\":\"yellow\"},194,[\"yellow\",139,\"green\",[\"blue\",14,\"green\",\"blue\",\"blue\",119,\"violet\",-5],{\"a\":61,\"b\":\"orange\"},\"violet\"]],\"f\":20},{\"c\":\"orange\",\"a\":{\"e\":135,\"c\":\"violet\",\"a\":111,\"g\":{\"a\":\"red\",\"b\":96},\"b\":186,\"d\":33,\"f\":127},\"b\":{\"e\":83,\"a\":\"yellow\",\"d\":\"orange\",\"c\":\"blue\",\"h\":\"orange\",\"b\":0,\"g\":\"orange\",\"f\":164,\"i\":\"blue\"},\"d\":-28},-42,{\"e\":\"green\",\"c\":{\"e\":-20,\"c\":\"yellow\",\"a\":66,\"b\":156,\"d\":\"violet\"},\"a\":[-9,129],\"g\":74,\"b\":{\"e\":\"violet\",\"c\":\"green\",\"a\":[52,\"blue\",[\"green\",-8,\"green\",\"green\"],\"red\",188,43,\"green\",{\"e\":\"orange\",\"a\":40,\"d\":-6,\"c\":\"orange\",\"h\":93,\"b\":\"green\",\"g\":103,\"f\":\"red\"}],\"g\":{\"e\":\"red\",\"c\":\"yellow\",\"a\":16,\"b\":7,\"d\":70},\"b\":{\"e\":133,\"a\":150,\"d\":{\"a\":34,\"b\":\"green\"},\"j\":166,\"c\":156,\"h\":79,\"b\":\"red\",\"g\":178,\"f\":-37,\"i\":163},\"d\":\"blue\",\"f\":\"green\"},\"d\":{\"c\":\"blue\",\"a\":\"violet\",\"b\":177,\"d\":80},\"f\":[61,88,\"yellow\",{\"c\":\"blue\",\"a\":\"orange\",\"b\":\"violet\",\"d\":\"yellow\"},{\"c\":119,\"a\":\"violet\",\"b\":{\"a\":\"red\"},\"d\":84},95,170]},{\"e\":191,\"c\":2,\"a\":\"orange\",\"b\":{\"a\":[-9,\"green\",\"violet\",[\"green\",132,\"red\",61,85],3,2]},\"d\":[\"green\",\"yellow\",\"violet\",-46,48,\"green\"]}],[\"violet\",[\"yellow\"],[\"blue\",{\"e\":-15,\"c\":{\"e\":[\"green\",\"violet\",0,3,183,165,-1,\"orange\",\"blue\"],\"a\":\"violet\",\"d\":20,\"c\":\"violet\",\"h\":\"yellow\",\"b\":60,\"g\":\"violet\",\"f\":163,\"i\":135},\"a\":71,\"b\":{\"c\":[115,\"green\",25,\"yellow\",\"blue\",66],\"a\":\"yellow\",\"b\":\"green\"},\"d\":\"violet\",\"f\":{\"e\":\"yellow\",\"c\":\"blue\",\"a\":\"blue\",\"b\":59,\"d\":[69,71,\"yellow\",\"red\",99,\"green\",\"yellow\",144,43,-38]}},\"yellow\",\"blue\",\"orange\",55,{\"c\":[-9,-16,\"green\",100,28,\"red\",\"blue\",\"blue\",174],\"a\":-31,\"b\":106,\"d\":\"violet\"},\"blue\"],[141,[32,\"orange\",{\"c\":-6,\"a\":-7,\"b\":64},[\"blue\",{\"e\":-24,\"c\":\"yellow\",\"a\":153,\"b\":\"orange\",\"d\":\"blue\",\"f\":\"violet\"},\"violet\",-28,197,\"yellow\",\"green\",\"green\"],2,{\"e\":\"violet\",\"c\":\"red\",\"a\":\"red\",\"b\":\"yellow\",\"d\":\"yellow\"},[\"yellow\"],\"red\"],[\"green\",[-16,47],\"blue\",87,\"red\",\"green\"],105,\"violet\",[[127,\"violet\",81],\"red\",\"blue\",[193,178,-6],20,\"red\",61],-2,\"blue\",-35],{\"c\":-12,\"a\":189,\"b\":\"red\"},[12,196,[\"red\",27,[\"violet\",\"green\",15,[\"yellow\",\"green\",152,56,187,\"yellow\",69],\"violet\"]],127,{\"a\":\"yellow\"},-35,[[180,{\"c\":\"yellow\",\"a\":191,\"b\":\"violet\",\"d\":\"red\"},41,33,-5],188,\"red\",\"violet\",23,100,30,91,-15],\"blue\"],[[129,\"blue\",{\"a\":\"red\"},\"violet\",\"green\",56,[\"yellow\"],180,[156,\"violet\"],-49],48,\"red\",38,{\"a\":{\"e\":93,\"a\":\"yellow\",\"d\":170,\"c\":{\"e\":116,\"a\":5,\"d\":89,\"j\":\"blue\",\"c\":\"blue\",\"h\":\"red\",\"b\":\"blue\",\"g\":-2,\"f\":\"red\",\"i\":-7},\"h\":148,\"b\":149,\"g\":\"red\",\"f\":-18,\"i\":-44}},101,183],[25,16,123]],{\"e\":{\"c\":\"orange\",\"a\":{\"e\":[{\"c\":\"green\",\"a\":\"orange\",\"b\":100,\"d\":-30}],\"a\":187,\"d\":\"green\",\"c\":{\"a\":122},\"h\":-10,\"b\":118,\"g\":-12,\"f\":63},\"b\":{\"e\":[\"violet\",\"yellow\",\"yellow\",167,163,5],\"a\":-28,\"d\":[-2,61,\"red\",-18,\"red\",{\"e\":\"yellow\",\"a\":\"orange\",\"d\":\"yellow\",\"j\":\"green\",\"c\":\"orange\",\"h\":-10,\"b\":-32,\"g\":115,\"f\":141,\"i\":164},\"red\",[\"violet\",99,\"orange\",\"blue\",\"orange\",\"green\",\"green\",\"violet\",\"yellow\"]],\"c\":-24,\"h\":\"blue\",\"b\":\"violet\",\"g\":47,\"f\":156},\"d\":195},\"a\":{\"a\":[176]},\"d\":[\"violet\",[[\"green\",180,\"violet\",\"yellow\"],{\"e\":133,\"a\":\"violet\",\"d\":{\"e\":57,\"a\":\"yellow\",\"d\":57,\"j\":\"violet\",\"c\":\"red\",\"h\":33,\"b\":\"green\",\"g\":\"yellow\",\"f\":\"green\",\"i\":79},\"j\":\"orange\",\"c\":\"violet\",\"h\":62,\"b\":\"blue\",\"g\":-37,\"f\":\"violet\",\"i\":93},-43,\"violet\",103,\"yellow\",194,56],{\"e\":-19,\"c\":\"yellow\",\"a\":\"orange\",\"b\":-19,\"d\":\"red\",\"f\":\"yellow\"}],\"c\":162,\"h\":[73,\"green\",[87,{\"a\":\"green\"}],[56,\"green\",[[\"green\",-2,\"green\",-47,\"yellow\",-39,47],\"red\",129,[90,181,50,\"green\",\"green\",\"green\",\"blue\",7,\"violet\"],-3,9,-12,171],\"red\",\"orange\",159,[\"violet\",\"yellow\",77,86,\"yellow\",\"yellow\",\"red\",185],145,[81,133]],\"yellow\",-3,[{\"c\":[\"orange\",108,82],\"a\":\"violet\",\"b\":{\"a\":\"yellow\",\"b\":\"yellow\"},\"d\":42}],\"orange\",[-27,[\"green\",{\"c\":\"violet\",\"a\":\"violet\",\"b\":\"orange\",\"d\":15},78],\"red\",23],\"orange\"],\"b\":{\"e\":\"yellow\",\"a\":46,\"d\":[118,31,142,{\"e\":-48,\"a\":\"blue\",\"d\":\"green\",\"c\":\"violet\",\"h\":69,\"b\":\"orange\",\"g\":178,\"f\":\"orange\",\"i\":\"green\"},{\"e\":109,\"a\":\"orange\",\"d\":-7,\"c\":42,\"h\":168,\"b\":\"blue\",\"g\":157,\"f\":{\"a\":93,\"b\":142},\"i\":38},[59,80],\"orange\",73,\"violet\"],\"c\":122,\"h\":{\"e\":153,\"c\":\"yellow\",\"a\":11,\"b\":\"orange\",\"d\":101},\"b\":\"blue\",\"g\":\"orange\",\"f\":[{\"e\":48,\"c\":-39,\"a\":77,\"g\":-33,\"b\":\"yellow\",\"d\":30,\"f\":36},153,{\"c\":\"violet\",\"a\":78,\"b\":63,\"d\":\"orange\"},117],\"i\":\"red\"},\"g\":\"yellow\",\"f\":{\"a\":\"orange\",\"b\":[{\"a\":87},\"violet\",\"orange\",96,154,\"orange\",\"violet\",{\"a\":[-45],\"b\":103}]}},\"red\"],[{\"e\":\"violet\",\"a\":{\"e\":167,\"a\":{\"e\":\"orange\",\"c\":{\"e\":\"red\",\"c\":76,\"a\":\"green\",\"b\":\"violet\",\"d\":146,\"f\":152},\"a\":\"violet\",\"b\":-8,\"d\":76,\"f\":\"red\"},\"d\":[102,\"yellow\",\"blue\",\"blue\",22,73],\"c\":\"red\",\"h\":{\"e\":{\"c\":92,\"a\":178,\"b\":{\"e\":-43,\"a\":\"yellow\",\"d\":136,\"j\":\"red\",\"c\":193,\"h\":98,\"b\":\"orange\",\"g\":49,\"f\":\"yellow\",\"i\":\"violet\"}},\"a\":54,\"d\":138,\"j\":[0,[177,178],\"red\",52,[87,\"violet\",123,\"orange\",\"orange\",\"yellow\",48,\"yellow\"],\"violet\",100,\"blue\"],\"c\":3,\"h\":\"green\",\"b\":175,\"g\":{\"e\":[175,-25,-47,\"orange\",60,185],\"a\":[\"orange\",-49,156],\"d\":\"yellow\",\"j\":8,\"c\":-28,\"h\":129,\"b\":[89,-12,67,\"green\",195,\"red\",\"violet\",150,\"red\",106],\"g\":\"violet\",\"f\":-29,\"i\":123},\"f\":\"orange\",\"i\":71},\"b\":[172,[\"yellow\",\"violet\",\"green\",\"blue\",194],-46,{\"a\":102,\"b\":\"green\"}],\"g\":[{\"e\":{\"c\":23,\"a\":\"yellow\",\"b\":-25},\"a\":\"blue\",\"d\":\"green\",\"j\":185,\"c\":\"yellow\",\"h\":[\"orange\",\"violet\",-21],\"b\":{\"c\":191,\"a\":197,\"b\":\"yellow\"},\"g\":115,\"f\":-41,\"i\":\"blue\"},-17,[-23,64,\"red\",8,\"orange\",[105,-11,29,-23,30,65,15],170],\"yellow\",\"yellow\",-46,\"green\",\"orange\",143],\"f\":177},\"d\":\"red\",\"c\":\"red\",\"h\":135,\"b\":{\"e\":\"red\",\"a\":\"orange\",\"d\":[\"violet\",[23,\"red\",\"violet\",\"orange\",66,{\"c\":\"orange\",\"a\":\"green\",\"b\":169,\"d\":57},\"blue\",125,\"green\"],110,135,[-40,\"violet\",\"yellow\",-26,-23,44],\"orange\",28],\"c\":\"orange\",\"h\":107,\"b\":91,\"g\":105,\"f\":{\"e\":{\"e\":164,\"c\":180,\"a\":\"blue\",\"b\":\"yellow\",\"d\":144},\"c\":\"violet\",\"a\":\"violet\",\"g\":95,\"b\":\"red\",\"d\":\"violet\",\"f\":\"green\"},\"i\":156},\"g\":[\"violet\",108,[\"blue\",\"yellow\",\"red\",[23,\"yellow\",3,159,112],{\"e\":-41,\"c\":\"green\",\"a\":22,\"b\":\"violet\",\"d\":\"blue\",\"f\":\"blue\"},\"violet\",-27,\"green\",\"violet\",-17],\"green\",{\"e\":{\"e\":[-19,96,-28,\"orange\"],\"c\":\"yellow\",\"a\":\"yellow\",\"g\":124,\"b\":97,\"d\":{\"a\":\"blue\"},\"f\":\"green\"},\"a\":46,\"d\":[\"blue\",118,\"yellow\",\"yellow\",\"yellow\",\"green\",[\"yellow\",-10,90,167,\"red\",54,-15]],\"j\":106,\"c\":\"red\",\"h\":[{\"e\":\"red\",\"c\":28,\"a\":\"yellow\",\"b\":170,\"d\":\"blue\",\"f\":105},-40,\"orange\",188,\"yellow\",142],\"b\":117,\"g\":\"violet\",\"f\":{\"a\":[-5,\"red\",46,182,\"red\",\"orange\"]},\"i\":\"yellow\"},[\"orange\",88,18,{\"e\":\"blue\",\"a\":\"violet\",\"d\":\"blue\",\"c\":\"violet\",\"h\":\"violet\",\"b\":196,\"g\":103,\"f\":67,\"i\":13}],\"blue\",\"blue\"],\"f\":-19,\"i\":{\"a\":166}},164],[{\"e\":{\"e\":\"blue\",\"a\":13,\"d\":{\"e\":{\"e\":\"orange\",\"a\":88,\"d\":\"red\",\"c\":\"yellow\",\"h\":[93,79],\"b\":\"orange\",\"g\":109,\"f\":34,\"i\":-13},\"a\":-44,\"d\":\"red\",\"j\":[173,78,\"red\",{\"e\":\"yellow\",\"a\":-32,\"d\":\"blue\",\"j\":\"violet\",\"c\":\"blue\",\"h\":119,\"b\":\"green\",\"g\":-30,\"f\":193,\"i\":95},\"orange\",-43,-16],\"c\":[\"green\",{\"e\":41,\"c\":\"red\",\"a\":109,\"b\":159,\"d\":59},173,18,\"violet\",21,\"red\"],\"h\":\"blue\",\"b\":44,\"g\":{\"a\":129,\"b\":-10},\"f\":-26,\"i\":27},\"j\":\"red\",\"c\":{\"a\":{\"c\":-28,\"a\":\"green\",\"b\":188,\"d\":\"blue\"},\"b\":\"blue\"},\"h\":[183,[118,[-7,\"orange\"],132,[23,175,\"yellow\",\"green\",11,178,171,\"orange\",\"blue\",18],134,1,\"green\",[-9,99],103,-25],\"red\",[65,\"red\",\"blue\"],\"violet\",\"blue\"],\"b\":\"yellow\",\"g\":164,\"f\":-9,\"i\":{\"c\":51,\"a\":\"green\",\"b\":115,\"d\":{\"a\":27,\"b\":\"red\"}}},\"a\":[{\"e\":[143,\"violet\",128,\"red\",\"yellow\",185,\"green\",\"red\",\"red\"],\"a\":29,\"d\":\"red\",\"c\":170,\"h\":[131,\"violet\",96,{\"a\":\"yellow\",\"b\":\"green\"},139,22,176,\"yellow\",[-46,-14,\"red\",\"blue\",83,141],[132,108,\"blue\",\"blue\",\"green\",197,\"yellow\"]],\"b\":\"blue\",\"g\":\"orange\",\"f\":\"yellow\",\"i\":[-9]},\"violet\",56,[169,12,155,[\"red\",197,{\"e\":\"violet\",\"a\":22,\"d\":\"violet\",\"c\":84,\"h\":\"red\",\"b\":70,\"g\":\"violet\",\"f\":-41},47,\"violet\"],[[\"green\",\"green\",179,56,\"green\",\"violet\",171,\"violet\",\"violet\"],\"blue\",\"red\",\"green\",-17,\"green\",190],\"green\",\"red\",146,60],\"yellow\",\"red\",\"yellow\",\"violet\"],\"d\":[[141,40,\"yellow\",1,\"blue\",\"green\",\"yellow\",{\"e\":13,\"a\":\"blue\",\"d\":\"red\",\"c\":\"red\",\"h\":176,\"b\":\"violet\",\"g\":164,\"f\":4,\"i\":\"violet\"}]],\"c\":72,\"h\":15,\"b\":\"yellow\",\"g\":{\"e\":[-12,\"blue\",[\"red\",\"blue\",11],29,{\"e\":59,\"c\":\"red\",\"a\":{\"e\":55,\"a\":\"blue\",\"d\":\"orange\",\"c\":\"yellow\",\"h\":\"violet\",\"b\":-19,\"g\":\"green\",\"f\":\"violet\",\"i\":197},\"b\":\"orange\",\"d\":\"violet\",\"f\":90},[-14,154,\"violet\",\"orange\",74,{\"e\":\"yellow\",\"a\":\"violet\",\"d\":66,\"c\":\"yellow\",\"h\":80,\"b\":\"yellow\",\"g\":\"yellow\",\"f\":\"orange\",\"i\":\"blue\"},\"green\",\"red\",116,149],\"green\",108],\"c\":{\"c\":28,\"a\":\"blue\",\"b\":\"yellow\",\"d\":\"blue\"},\"a\":36,\"b\":[\"orange\",\"green\",\"orange\",\"green\",\"red\",46,55,\"blue\",[\"violet\",98,[163,-35,163,-28],\"blue\",\"red\",155,\"blue\"],-8],\"d\":163},\"f\":[9,\"green\",{\"c\":\"green\",\"a\":\"violet\",\"b\":68,\"d\":\"yellow\"},114,33,1,-25]},[\"red\",[-20,{\"c\":\"yellow\",\"a\":\"red\",\"b\":\"green\",\"d\":{\"a\":\"red\"}},\"red\",[[141,76],[174],100,{\"e\":126,\"c\":39,\"a\":[\"violet\",94,\"orange\",102,\"blue\"],\"b\":55,\"d\":\"yellow\",\"f\":\"yellow\"},146,{\"c\":169,\"a\":\"red\",\"b\":\"red\"},[[\"green\",-48,\"violet\",\"orange\"],[80,-7,-22,\"yellow\",\"orange\",\"yellow\",185,\"orange\"],\"green\",\"violet\",\"orange\"],\"red\"],\"yellow\"],[{\"a\":[\"orange\",\"blue\"],\"b\":[{\"a\":-42,\"b\":\"violet\"},\"green\",99,-20,\"blue\"]},{\"c\":\"blue\",\"a\":\"violet\",\"b\":14,\"d\":9},\"green\",{\"c\":[\"blue\",148,[38,139,125,52,\"red\",40,190,\"yellow\",21,\"violet\"],\"violet\",110,\"green\"],\"a\":{\"c\":97,\"a\":[35,\"orange\",44,\"red\",87,\"orange\",\"blue\",61,\"yellow\"],\"b\":176,\"d\":144},\"b\":137},85,[192,-37,\"orange\",{\"c\":\"yellow\",\"a\":-10,\"b\":[71]},\"yellow\",176,[\"green\",14],{\"a\":102},-39],\"violet\",164],-9,\"blue\",[[\"blue\"],70]]]]".

day_13_input() ->
  "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 81 happiness units by sitting next to Carol.
Alice would lose 42 happiness units by sitting next to David.
Alice would gain 89 happiness units by sitting next to Eric.
Alice would lose 89 happiness units by sitting next to Frank.
Alice would gain 97 happiness units by sitting next to George.
Alice would lose 94 happiness units by sitting next to Mallory.
Bob would gain 3 happiness units by sitting next to Alice.
Bob would lose 70 happiness units by sitting next to Carol.
Bob would lose 31 happiness units by sitting next to David.
Bob would gain 72 happiness units by sitting next to Eric.
Bob would lose 25 happiness units by sitting next to Frank.
Bob would lose 95 happiness units by sitting next to George.
Bob would gain 11 happiness units by sitting next to Mallory.
Carol would lose 83 happiness units by sitting next to Alice.
Carol would gain 8 happiness units by sitting next to Bob.
Carol would gain 35 happiness units by sitting next to David.
Carol would gain 10 happiness units by sitting next to Eric.
Carol would gain 61 happiness units by sitting next to Frank.
Carol would gain 10 happiness units by sitting next to George.
Carol would gain 29 happiness units by sitting next to Mallory.
David would gain 67 happiness units by sitting next to Alice.
David would gain 25 happiness units by sitting next to Bob.
David would gain 48 happiness units by sitting next to Carol.
David would lose 65 happiness units by sitting next to Eric.
David would gain 8 happiness units by sitting next to Frank.
David would gain 84 happiness units by sitting next to George.
David would gain 9 happiness units by sitting next to Mallory.
Eric would lose 51 happiness units by sitting next to Alice.
Eric would lose 39 happiness units by sitting next to Bob.
Eric would gain 84 happiness units by sitting next to Carol.
Eric would lose 98 happiness units by sitting next to David.
Eric would lose 20 happiness units by sitting next to Frank.
Eric would lose 6 happiness units by sitting next to George.
Eric would gain 60 happiness units by sitting next to Mallory.
Frank would gain 51 happiness units by sitting next to Alice.
Frank would gain 79 happiness units by sitting next to Bob.
Frank would gain 88 happiness units by sitting next to Carol.
Frank would gain 33 happiness units by sitting next to David.
Frank would gain 43 happiness units by sitting next to Eric.
Frank would gain 77 happiness units by sitting next to George.
Frank would lose 3 happiness units by sitting next to Mallory.
George would lose 14 happiness units by sitting next to Alice.
George would lose 12 happiness units by sitting next to Bob.
George would lose 52 happiness units by sitting next to Carol.
George would gain 14 happiness units by sitting next to David.
George would lose 62 happiness units by sitting next to Eric.
George would lose 18 happiness units by sitting next to Frank.
George would lose 17 happiness units by sitting next to Mallory.
Mallory would lose 36 happiness units by sitting next to Alice.
Mallory would gain 76 happiness units by sitting next to Bob.
Mallory would lose 34 happiness units by sitting next to Carol.
Mallory would gain 37 happiness units by sitting next to David.
Mallory would gain 40 happiness units by sitting next to Eric.
Mallory would gain 18 happiness units by sitting next to Frank.
Mallory would gain 7 happiness units by sitting next to George.".

day_14_input() ->
  "Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
Cupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.
Rudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.
Donner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.
Dasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.
Blitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
Prancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.
Comet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
Vixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds.".

day_15_input() ->
  "Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1".
