-module(santa).

-export([day_1a/0, day_1b/0,
         day_2a/0, day_2b/0,
         day_3a/0, day_3b/0,
         day_4a/0, day_4b/0,
         day_5a/0, day_5b/0,
         day_6a/0, day_6b/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

day_1a() ->
  get_distance(day_1a_input()).

get_distance(List) ->
  {_FinalDirection, {FinalSumX, FinalSumY}} =
    lists:foldl(
      fun(Input, FractionState) ->
          make_new_coordinates(FractionState, Input)
      end,
      {north, {0, 0}}, string:tokens(List, ", ")),
  abs(FinalSumX) + abs(FinalSumY).

make_new_coordinates({north, {X, Y}}, [$R  | Num]) ->
  {east, {X + list_to_integer(Num), Y}};
make_new_coordinates({east, {X, Y}}, [$R  | Num]) ->
  {south, {X, Y + list_to_integer(Num)}};
make_new_coordinates({south, {X, Y}}, [$R  | Num]) ->
  {west, {X - list_to_integer(Num), Y}};
make_new_coordinates({west, {X, Y}}, [$R  | Num]) ->
  {north, {X, Y - list_to_integer(Num)}};
make_new_coordinates({north, {X, Y}}, [$L  | Num]) ->
  {west, {X - list_to_integer(Num), Y}};
make_new_coordinates({west, {X, Y}}, [$L  | Num]) ->
  {south, {X, Y + list_to_integer(Num)}};
make_new_coordinates({south, {X, Y}}, [$L  | Num]) ->
  {east, {X + list_to_integer(Num), Y}};
make_new_coordinates({east, {X, Y}}, [$L  | Num]) ->
  {north, {X, Y - list_to_integer(Num)}}.

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

day_3a() ->
  find_triangles(string:tokens(day_3a_input(), "\n")).

find_triangles(List) ->
  find_triangles(List, 0).

find_triangles([], Count) ->
  Count;
find_triangles([Triangle | Rest], Count) ->
  find_triangles(Rest,
                 case is_triangle(
                        lists:map(fun list_to_integer/1,
                                  string:tokens(Triangle, " "))) of
                   true ->
                     Count + 1;
                   false -> Count
                 end).

is_triangle(Input) ->
  [A, B, C] = lists:sort(Input),
  A + B > C.

day_3b() ->
  find_triangles_vertically(string:tokens(day_3a_input(), " \n")).

find_triangles_vertically(List) ->
  find_triangles_vertically(List, 0).

find_triangles_vertically([], Count) ->
  Count;
find_triangles_vertically([A1, B1, C1,
                           A2, B2, C2,
                           A3, B3, C3 | List], Count) ->
  find_triangles_vertically(
    List, Count +
      find_triangles([A1 ++ " " ++ A2 ++ " " ++ A3,
                      B1 ++ " " ++ B2 ++ " " ++ B3,
                      C1 ++ " " ++ C2 ++ " " ++ C3])).

day_4a() ->
  lists:foldl(fun(Line, Sum) ->
                  {Input, Number, Key} = split_input(Line),
                  case Key =:= decipher(Input) of
                    true ->
                      Sum + list_to_integer(Number);
                    false ->
                      Sum
                  end
              end, 0, string:tokens(day_4a_input(), "\n")).

decipher(List) ->
  limit_to_five(get_most_common_letters(List)).

get_most_common_letters(List) ->
  get_letters(sort_by_number(maps:to_list(count_letters(List)))).

count_letters(List) ->
  count_letters(List, #{}).

count_letters([], Count) ->
  Count;
count_letters([$- | Rest], Count) ->
  count_letters(Rest, Count);
count_letters([Letter | Rest], Count) ->
  count_letters(Rest, maps:put(Letter, maps:get(Letter, Count, 0) + 1, Count)).

sort_by_number(List) ->
  lists:sort(fun ({_A1, N1},{_A2, N2}) when N1 > N2 -> true;
                 ({A1, N1}, {A2, N1}) when A1 < A2 -> true;
                 ({_A1, _N1}, {_A2, _N2}) -> false end, List).

get_letters(List) ->
  lists:map(fun({L, _N}) ->
                L end, List).

limit_to_five(List) when length(List) =< 5 -> List;
limit_to_five(List) ->
  {L5, _} = lists:split(5, List),
  L5.

split_input(List) ->
  [Input, Key] = string:tokens(List, "01234567890[]"),
  [NumberPart, _] = string:tokens(List, "["),
  Number = string:substr(NumberPart, string:rstr(NumberPart, "-") + 1),
  {Input, Number, Key}.

day_4b() ->
  lists:foldl(fun(Line, Sum) ->
                  {Input, Number, Key} = split_input(Line),
                  case Key =:= decipher(Input) of
                    true ->
                      case
                        "northpole object storage " =:= rotate(Input, list_to_integer(Number)) of
                        true ->
                          list_to_integer(Number);
                        false ->
                          Sum
                      end;
                    false ->
                      Sum
                  end
              end, 0, string:tokens(day_4a_input(), "\n")).

rotate(List, Number) ->
  rotate(List, Number, []).

rotate([], _, Output) ->
  lists:reverse(Output);
rotate([$- | T], N, Output) ->
  rotate(T, N, " " ++ Output);
rotate([L | T], N, Output) when L + N > $z ->
  rotate(T, N, [$a + ((L + N -$a) rem ($z-$a+1))] ++ Output);
rotate([L | T], N, Output) ->
  rotate(T, N, [L + N] ++ Output).

day_5a() ->
  find_password([], 0).

find_password(List, _) when length(List) == 8 ->
  lists:reverse(List);
find_password(List, Number) ->
  case generate_hash(Number) of
    [$0, $0, $0, $0, $0, PassKey | _Rest] ->
      find_password([PassKey | List], Number + 1);
    _Else -> find_password(List, Number + 1)
  end.

generate_hash(Number) ->
  lists:flatten(
    lists:map(
      fun (I) -> N = integer_to_list(I, 16),
                 normalize_two_characters(N)
      end, binary_to_list(
             crypto:hash(md5,
                         "wtnhxymk"
                         ++ integer_to_list(Number, 10))))).

normalize_two_characters(N) when length(N) == 1 ->
  "0" ++ N;
normalize_two_characters(N) ->
  N.

day_5b() ->
  find_fancy_password(#{}, 0, 0).

find_fancy_password(Map, _, Found ) when Found == 8 ->
  extract_key_sequence(Map);
find_fancy_password(Map, Number, Found) ->
  {NewMap, NewFound} =
    case generate_hash(Number) of
      [$0, $0, $0, $0, $0, Index, PassKey | _Rest] ->
        case is_correct_index(Index) of
          true ->
            case is_already_found(Index, Map) of
              false ->
                {maps:put(Index, PassKey, Map), Found + 1};
              true ->
                {Map, Found}
            end;
          false  ->
            {Map, Found}
        end;
      _Else -> {Map, Found}
    end,
  find_fancy_password(NewMap, Number + 1, NewFound).

extract_key_sequence(Map) ->
  lists:map(fun({_A, B}) -> B end, lists:sort(maps:to_list(Map))).

is_already_found(Index, Map) ->
  lists:member(Index, maps:keys(Map)).

is_correct_index(Index) -> lists:member(Index, lists:seq($0,$7)).

day_6a() ->
  lists:map(fun (List) ->
                hd(get_most_common_letters(List))
            end, make_vertical(day_6a_input())).

make_vertical(Input) ->
  make_vertical(Input, #{}, 1).

make_vertical([], Output, _Position) ->
  maps:values(Output);
make_vertical([$\n | Rest], Output, _Position) ->
  make_vertical(Rest, Output, 1);
make_vertical([Letter | Rest], Output, Position) ->
  PositionValue = maps:get(Position, Output, []),
  make_vertical(Rest, maps:put(Position, [Letter] ++ PositionValue, Output),
                Position + 1).

day_6b() ->
  lists:map(fun (List) ->
                hd(lists:reverse(get_most_common_letters(List)))
            end, make_vertical(day_6a_input())).

%% Inputs

day_1a_input() ->
  "R4, R3, R5, L3, L5, R2, L2, R5, L2, R5, R5, R5, R1, R3, L2, L2, L1, R5, L3, R1, L2, R1, L3, L5, L1, R3, L4, R2, R4, L3, L1, R4, L4, R3, L5, L3, R188, R4, L1, R48, L5, R4, R71, R3, L2, R188, L3, R2, L3, R3, L5, L1, R1, L2, L4, L2, R5, L3, R3, R3, R4, L3, L4, R5, L4, L4, R3, R4, L4, R1, L3, L1, L1, R4, R1, L4, R1, L1, L3, R2, L2, R2, L1, R5, R3, R4, L5, R2, R5, L5, R1, R2, L1, L3, R3, R1, R3, L4, R4, L4, L1, R1, L2, L2, L4, R1, L3, R4, L2, R3, L1, L5, R4, R5, R2, R5, R1, R5, R1, R3, L3, L2, L2, L5, R2, L2, R5, R5, L2, R3, L5, R5, L2, R4, R2, L1, R3, L5, R3, R2, R5, L1, R3, L2, R2, R1".

day_2a_input() ->
  "RLRDDRLLDLRLUDDULLDRUUULDDLRLUDDDLDRRDUDDDLLURDDDLDDDRDURUDRDRRULUUDUDDRRRLRRRRRLRULRLLRULDRUUDRLRRURDDRLRULDLDULLLRULURRUULLRLLDDDDLLDURRUDLDLURDRDRDLUUUDDRDUUDDULLUURRDRLDDULURRRUDLLULULDLLURURUDRRRRUDRLRDLRRLDDRDDLULDLLLURURDUDRRRRUULURLRDULDRLUDRRUDDUULDURUDLDDURRRDLULLUUDRLLDUUDLDRUDDRLLLLLLDUDUDDLRDLRRDRUDDRRRLLRRDLLRLDDURUURRRDDLDUULLDLDLRURDLLLDDRUUDRUDDDDULRLLDUULRUULLLULURRRLLULDLDUDLDLURUDUDULLDLLUUDRRDRLUURURURURDLURUUDLDRLUDDUUDULDULULLLDLDDULLULLDULRRDRULLURRRULLDDDULULURLRDURLLURUDDULLRUDLRURURRDRDUULDRUUDURDURDDLRDUUULDUUDRDURURDRRRURLLDDLLLURURULULUDLRDLDRDRURLRLULRDLU
UDLDURRULDRDDLDUULUDLDUULUURDDRUDRURRRUDRURLLDDRURLDLRDUUURDLLULURDDUDDDRRRURLLDLDLULRDULRLULDLUUDLLRLDLRUUULDDUURDLDDRRDLURLDUDDRURDRRURDURRRLUULURDDLRDLDRRRLDUDRLRLLRLDDUULDURUUULLLRRRRRRRDRRRDRLUULDLDDLULDRDUDLLUDRRUDRUUDULRLUURDDDDRRUUDLURULLLURDULUURDRDDURULRUDRRDLRDUUUUUDDDRDRDDRUDRDDDRLRUUDRDRDDDLUDRDRLDRDDRULURDRLDRUDUDRUULRLLUDRDRLLLLDUDRRLLURDLLLDRRUDDUDRLRLDUDRLURRUUULURDDRUURRLDRLRRRUUDLULDDDRDLDUUURLLUULDDRRUDLDDRUDUDUURURDDRDULLLLLULRRRDLRRRDDDLURDDDDLUULLLRDDURRRRLURRLDDLRUULULRDRDDDDLDUUUUUUDRRULUUUDD
UURDRRUDLURRDDDLUDLRDURUDURDLLLLRDLRLRDDRDRDUUULRDLLDLULULRDUDDRRUUDURULDLUDLRDRUDLDDULLLDDRDLLDULLLURLLRDDLDRDULRRDDULRDURLLRUDRLRRLUDURLDRDLDLRLLLURLRRURDLDURDLUDULRDULLLDRDDRDLDRDULUULURDRRRLDRRUULULLDDRRLDLRUURLRUURLURRLLULUUULRLLDDUDDLRLDUURURUDLRDLURRLLURUDLDLLUDDUULUUUDDDURDLRRDDDLDRUDRLRURUUDULDDLUUDDULLDDRRDDRRRUDUDUDLDLURLDRDLLLLDURDURLRLLLUUDLRRRRUDUDDLDLRUURRLRRLUURRLUDUDRRRRRRRLDUDDRUDDLUDLRDDDRLDUULDRDRRDLDRURDLDRULRLRLUDRDLRRUURUUUUDLDUUULLLRRRRRDLRRURDDLLLLUULDLLRULLUDLLDLLUDLRLRRLRURDDRRL
URDRDLLRDDDLLLDDLURLRURUURRRLUURURDURRLLUDURRLRLDLUURDLULRRDRUDDLULDLDRLDLRLRRLLLDDDUDDDLRURURRLLDRRRURUDLRDDLLDULDDLDRLUUUDRRRULDUULRDDDLRRLLURDDURLULRDUDURRLLDLLRLDUDDRRDDLRLLLDUDRLUURRLLDULRLDLUUUUUDULUDLULUDDUURRURLDLDRRLDLRRUDUDRRDLDUDDLULLDLLRDRURDRDRRLDDDDRDDRLLDDDLLUDRURLURDRRRRRUDDDUDUDDRDUUDRRUDUDRLULDDURULUURUUUURDRULRLRULLDDRRRUULRRRRURUDLDLRDLLDRLURLRUULLURDUDULRRURLRLLRRLLLURULRRRLDDUULLUUULRRDRULUUUUDRDRRDLRURLRLLRLRRRDRDRLDLUURUURULLDLULRRLRRDRULRRLLLDDURULLDLDLDLUUURDLDLUUDULRLLUDDRRDLLDLDLDURLUURRDDRRURDRLUDRLUUUDLDULDLUDRLDUDDLLRUDULLLLLDRRLLUULLUUURRDDUURDLLRDDLRLLU
LDUDRRDLUUDDRLLUUULURLDUDLUDLRLDRURLULRLLDDLRRUUUDDDDRDULDDUUDLRUULDRULLRDRUDDURLDUUURRUDUDRDRDURRDLURRRDRLDLRRRLLLRLURUURRDLLRDLDDLLRDUDDRDUULRULRRURLUDDUDDDUULLUURDULDULLLLRUUUDDRRRLDDDLDLRRDRDRDLUULRLULDRULDLRDRRUDULUDLLUDUULRDLRRUUDDLLDUDDRULURRLULDLDRRULDDRUUDDLURDLRDRLULRRLURRULDUURDLUDLLDRLDULLULDLLRDRDLLLUDLRULLRLDRDDDLDDDLRULDLULLRUUURRLLDUURRLRLDUUULDUURDURRULULRUUURULLLRULLURDDLDRLLRDULLUDLDRRRLLLLDUULRRLDURDURDULULDUURLDUDRLRURRDLUUULURRUDRUUUDRUR".

day_3a_input() ->
  "  566  477  376
  575  488  365
   50   18  156
    558  673  498
    133  112  510
    670  613   25
    84  197  643
    910  265  611
    894  252  545
    581    3  598
    98  742  574
    628  746  193
    129  677  265
    187  445  169
    288  242  128
    569  744  439
    685  748  471
    256   23  157
    218  343  491
    777  905  633
    778  867  840
    672  772  947
    500  763  420
    449  665  653
    23  558  858
    745  407  904
    766  194  576
    11  541  423
    179  690  274
    174  747  279
    741  538  336
    507  698  667
    689  920  467
    405  861   35
    575  631  992
    317  573  981
    817  536   44
    816  205  653
    45  631  683
    509  300  418
    712  192  865
    666  133  859
    32  331   54
    572  432  259
    552  456  267
    766  931  643
    902  840  557
    465  318  175
    491  863  329
    463  795  564
    589  213  805
    340  323  123
    602  812  499
    898  931  555
    899  835  341
    316  487  789
    677  790  797
    341  502  123
    502  196  136
    702  617   82
    939  581  362
    801  834  951
    464  941  843
    7  756  971
    273  297  661
    270  620  387
    776   96   80
    891  125   92
    376  175  141
    435  247  949
    719  586  478
    578  662  801
    446  202  556
    596  336    5
    855  202  558
    541  314   17
    43  295  959
    507  433  942
    380  721  579
    313  670  629
    301  341   59
    338  776  161
    202  291  844
    528  564  736
    411  765  465
    489   98  611
    301  817  959
    895  836  890
    249  313  267
    726  976  881
    300  601  657
    985   91  438
    925  596  905
    250  850  331
    104  714  258
    312  189  196
    36   62  611
    218  756  600
    225  803  736
    765  250  259
    768  438  728
    268  613  578
    951  816  919
    706  923  495
    430  487  915
    416  964  623
    737  699  581
    836  981   90
    740  471  498
    17  781  602
    732  527  191
    57  126  235
    778  344  370
    822  398  366
    245  597  127
    407  658  902
    542  545  985
    786  663   93
    872  512  690
    897  151  655
    226  362  164
    257  629  168
    94  280   86
    577  611   45
    648  756  953
    89  556  924
    116  330  373
    730  297  713
    836  630  486
    431   10  177
    350   21  950
    442   34  831
    219  115  125
    259  111  836
    56   13  825
    837  648  336
    414  314  670
    787  906  792
    232  526  550
    220  409  216
    383  241  720
    101  724    8
    108  695  885
    23   33  894
    458  827  435
    489  664  675
    272  542  261
    18  250  218
    896   60  659
    906  284  823
    179  668  925
    174  362  227
    211  589  911
    514  333  704
    575  716  896
    762  382  597
    344  783  294
    408  795  209
    91  119  110
    717  616  307
    760  106  333
    374  563  514
    454   51  709
    564  499  158
    566  541  601
    901  327  137
    417  859  921
    726  627  828
    534  416  475
    221  533  310
    731  423  274
    534  636  902
    463  513  606
    921  148  829
    314  151  657
    454  238  935
    468  244  703
    253  124  615
    427  524  804
    194  545  380
    913  998  210
    203  797  141
    993  320  265
    208  144  488
    791  710  361
    953  637  413
    372  472  926
    686  216  677
    775  476  473
    683  224  524
    670  424  556
    286  642  660
    578  964  895
    715  938  675
    727   48  846
    386  538  362
    271  702  241
    620  537  487
    689  388  118
    223  315  599
    477  445  509
    369  787  300
    645  679  536
    613  461  511
    586  730  876
    597  586  541
    745  452  960
    577   77  581
    733  177  562
    923  191   64
    521  751  297
    422  780  709
    521  887  550
    808  681  829
    907  938  753
    507  307  991
    752  386   22
    684  270  155
    91  234  151
    568  950  170
    651  954  210
    789   31  307
    550   71  560
    463  715  400
    491  765  600
    131  455  904
    592  453  555
    559  788  676
    391  484  363
    405  404  280
    68  114  556
    307  518  632
    49  549  907
    288  287  482
    729   36  395
    440  768  636
    838  772  889
    99  231  794
    181   81  684
    252  253  843
    738  717  531
    125  311  536
    727  508  487
    992   76  234
    680  114  100
    501  165  292
    597  562  167
    223  887  429
    613  345  482
    209   45  787
    98  175  821
    178  196  252
    345  127  459
    767  896  750
    601  965  522
    792  541  699
    989  350  109
    337  799  588
    276  531  244
    461  655  162
    413  284  340
    327   93  556
    431  873  989
    164  826  956
    279  486  578
    488  275  248
    672  462  539
    636  448   39
    116  504  619
    550  353  618
    803  506    5
    641  482  513
    190  973  509
    4  105  745
    443  619  243
    440  519  754
    624   62  907
    644  231  907
    232  238  900
    866  749  665
    490  988  547
    815  482  135
    331  992  234
    252  812  623
    95  661  698
    15  378   91
    986  338  255
    979  205  218
    927   59  302
    161  231  250
    855  250  389
    588  356   40
    279   90  105
    707  273   77
    846  311  842
    870  305  833
    77  477  346
    638  496  395
    642  374  864
    73  526  717
    851  399  892
    873  444  889
    988  133  217
    950  702  591
    989  338  278
    307  628  794
    815  568  390
    974  725  622
    463  296  984
    605  409  526
    665  554  473
    106  852  381
    874  597  620
    481  609  245
    604   18  655
    702  611  814
    711  341  254
    130  458  865
    677  689  278
    926  675  223
    363   15  154
    189  785  793
    189  556  925
    92  332  617
    1  884  831
    176   37  482
    174  856  362
    432  978  371
    957  662  493
    749  873  714
    259  938  328
    692  837  649
    721  949  456
    488  304  194
    270  776  448
    446  879  257
    647  705  462
    720  305   25
    230  489  454
    920  303  582
    325   20  478
    970  303  168
    628   29  916
    398  209  489
    248  194  426
    426  276  899
    14  274  981
    430   12  799
    229  235  745
    268  663  528
    283  439  906
    542  983  253
    214  938  241
    575  123  228
    704  628  326
    567  678  468
    439  984  300
    580  228  520
    229  668  370
    512  701  624
    267  912  271
    666  839  776
    804  362  559
    400  565  127
    418  349  450
    136  671  460
    302  515  931
    185  764  911
    441  574  331
    902  856  448
    123  934  288
    890  143  683
    307  470  748
    339  169  127
    114  339  834
    789  639  631
    676   82  453
    247  563  394
    436  655  703
    352  906  631
    277  367  327
    585  757  427
    545  710  988
    153  705  689
    98  300  882
    140  491  135
    193  220  911
    371  739  602
    538  419  864
    280  786  352
    795  750  697
    808  341   79
    361  606  668
    57  277  452
    524  267  976
    525   45  762
    462  885  782
    90  837  156
    510  394  826
    617   23  812
    443  755  483
    255  756  866
    931  404  762
    184  496  236
    841  792  640
    932  259  869
    893  697  951
    37  451  493
    939  592  146
    789  104   25
    477  617  150
    427  506   86
    723  243  717
    795  705  638
    908  506  556
    741  755  408
    608  852  240
    613  717  393
    392   59  448
    877  724  100
    61  141   24
    101  257  130
    139  313  147
    999  130  568
    82  256  539
    941  183  193
    279  793  217
    628  347  824
    503  506  905
    646  629  909
    785  518  510
    510  199  701
    413  201  703
    399  548  247
    11  396  606
    250  981  822
    314  758  810
    513  476  139
    830  232   74
    448  693  690
    503  556  646
    604  448  579
    21  441  367
    603  155  428
    589  655   64
    524  726  849
    262  556  808
    693   19  969
    102  306   91
    773  287  939
    49    2  177
    937   54  697
    967   51  745
    388  262  356
    193  636  387
    554  537  478
    510  689  720
    693  670  665
    905   32  730
    516  420  525
    109  627  935
    421  282  693
    504  374  435
    241  619  662
    277  809  333
    319   43  766
    787  803  267
    941  828  722
    710  489   10
    654  395  148
    95  733  154
    270  163  706
    247  564  771
    269  635  204
    560  604  137
    379  164   72
    282  557  149
    871  461  499
    140  475  289
    816   12  533
    974  361  166
    769  545  848
    543  493  875
    475  192  926
    342  564  976
    501  448  782
    94  172   75
    275  856  267
    215  853  210
    41  496  331
    829  583  434
    818  306  478
    660  431  552
    509  716  355
    404  812  211
    549   63  271
    422  642  655
    397  590  389
    698  235  288
    319   95  603
    810  218  336
    709  590   89
    749  734  884
    118  896  862
    550  398  700
    505  207  165
    449  339  778
    664  929  821
    724  926  630
    89   21  391
    698  176  786
    718  479  938
    391  577  266
    634  178   29
    56  373  972
    687  259  969
    98  480  250
    951  134  237
    939  491   15
    899  303  834
    252  416  295
    887  537  926
    496  232  609
    130  534  575
    519  436  934
    639  648   29
    625  127  541
    291  544  511
    744  570  404
    430  772  439
    381  304  231
    525  527  785
    137  802  499
    542  632  966
    637  482   42
    207  643  532
    527  816  501
    194  982  271
    862  943  367
    846  217  324
    872  894  184
    911  776  718
    939  849  594
    745  354  472
    838  305  284
    744  157  759
    436  263  650
    605  326  348
    319  530  463
    639  358  479
    675  826  732
    609  682  912
    97  714  874
    848  330  858
    874  646   24
    508  518  881
    567  481  576
    431  561  603
    934  715  700
    833  311  198
    323  661  724
    997  162  319
    93  395  412
    977  497  232
    858  448  225
    900  316  259
    107  458  401
    397  723  817
    720  328   17
    346  631  802
    824  567  852
    810  234  269
    251  784  926
    490  316  317
    637  291  350
    593  583  602
    450  116  310
    907  210  231
    904  264  396
    612  221  267
    144  156  136
    683  248  293
    64  786  842
    52  606  511
    26  192  927
    830  422  399
    956  349  575
    254  130  660
    431  515  161
    961  522  198
    995  873   43
    967  855  268
    487  898  564
    595  378  636
    707  203  268
    710  226  775
    277  120  551
    675  631  758
    203  160  294
    873  667  637
    279  597  765
    26  667  338
    305  963  550
    163  858  901
    328  832  537
    204   25  494
    601  307  594
    71  405  613
    642  691  258
    341  439  293
    93  470  627
    361   30  430
    942   70  305
    831  963  304
    368  925   25
    115   82  139
    53  185  545
    160  247  536
    462  260  666
    296  696   84
    465  683  683
    591  228  220
    743  744  432
    165  758  559
    651  933   87
    872   37  514
    340  970  512
    576  654  416
    27  383  888
    567  840  586
    722  822  425
    657  872  880
    91  453  574
    28  970  437
    955  274  342
    933  753  172
    582  836  289
    642  966  727
    240  298  925
    253  241  795
    115  374   68
    354  162  821
    283  587  328
    627  566  650
    712  159  846
    365  973  654
    848  281   89
    901  900  568
    211  583  905
    296  480  895
    433  337   45
    229  741  115
    249  675  433
    179  507  470
    796  121   16
    963  523  101
    469  485  112
    343  846    7
    391  733  732
    89  419  735
    571  797   25
    785  740  519
    436  198  533
    96  387  856
    250  391  421
    215   15  842
    389  128  120
    323  673  729
    87  583  624
    807  454  314
    970  227  247
    652  230  405
    391  534  677
    993  253  880
    747  688  284
    492  932  421
    290  800  344
    696  151  586
    535   61  371
    493  455  676
    138  417  892
    851  568  234
    480  639  309
    903  829  404
    662   85  687
    703  112  607
    410  151  995
    275  465  774
    97  579  152
    301  516  913
    960  436  672
    870  230  800
    151  263  674
    97  515  754
    763   67  705
    720  516  398
    817  696  171
    770   89  606
    655  660  736
    536  236  421
    713  104  578
    607  296  729
    432  222  852
    626  751  769
    529  954  670
    927   18   64
    559  840  376
    592  819  312
    423  489  842
    766   14  657
    350  498  198
    777  199  736
    767  639  924
    17  684  692
    652  739  958
    735  874  486
    175  308  988
    625  615  601
    125  235  645
    752  791   50
    263  568  505
    475  307  580
    241  679  757
    89  812  852
    133  824  507
    94  396  868
    285  756  877
    424  811  336
    587  202  550
    623  402  145
    169  730  142
    652  649   22
    42  181   68
    160   20  161
    132  166  179
    566  277  716
    521  381  640
    168  212  123
    489  333  741
    62  328   71
    506   16  721
    360  388  194
    516  412  767
    713  686  964
    711  765  514
    959  285  600
    759  610  862
    715  308  357
    348   41   10
    731  306  360
    902  727  296
    867  770  187
    63  794  254
    196  410  505
    693  527  570
    725  386  119
    322  796   62
    480  514   92
    514  825   38
    346  422  647
    210  455  853
    142   40  340
    298  382  190
    433  867  205
    346  489  315
    860  258  369
    845  976  997
    114  974  851
    168  441  671
    868  467  717
    824   26  616
    911  581  779
    853  127   81
    640  681  776
    926  331  747
    576  681  846
    626  673  515
    361  462  553
    188  968  946
    512  933  571
    357  238  647
    698  738  668
    786  602  599
    946  216  264
    700  485  646
    553  382  781
    180  127  629
    637  622  628
    798  504  116
    337   59  665
    701  287  318
    389  251  716
    570  551  491
    256  889  465
    665  720  734
    630  801  550
    637  699  547
    319  277  130
    807  803  576
    231  314  966
    944  813  672
    510  539  729
    339  673  746
    829  616   34
    320  151   16
    318  128  482
    4  146  473
    226  777  102
    207  731  253
    139  162  344
    134  404  641
    564  974  592
    569  771  878
    570  646  794
    589  126  159
    507  754  908
    832  358  827
    572  251  974
    922  454  938
    964  819   26
    963  150  603
    50  916  630
    578   63  306
    333  897  826
    716  886  733
    93  726  208
    876  729  569
    876    3  755
    271  294  734
    886  677  723
    643  688  273
    896  404  627
    991  524  399
    155  165  685
    195  634  821
    650  277  711
    459  697  131
    436  855  749
    196   97  765
    529  765  313
    781  986   44
    966  768  340
    218  706  304
    489  179  646
    61  267  435
    466  364  718
    392  149  666
    467  152  791
    180  286  624
    151  170  372
    870  267  612
    897  107  768
    2  487  286
    902  669  833
    902  742  635
    397  258  175
    742   95  866
    957  213  717
    723  555  749
    528  527   14
    203   83  762
    247  428   68
    656  153  208
    644  496  268
    14  641   16
    515  670  176
    523   58  192
    424  617   72
    766  588  999
    653  202  943
    219  567  386
    177  799  633
    229  499  572
    579  445  767
    365  247  682
    771  518  125
    310  151  329
    849  782  365
    613  769  364
    479  495  603
    551  260  105
    717  326  650
    476   48  883
    537  368  839
    136  321  105
    370  683  482
    396  576  186
    58  549  594
    37  532  381
    940  248  161
    915  546  228
    629  860   96
    672  121  741
    283  932  834
    162  737  544
    66  967  333
    199  916  591
    880  571  265
    953  573  132
    224   51  196
    562  494  245
    853   94  317
    550  519   80
    349  741  972
    814  201  949
    464  779  408
    32  790  544
    263  785  261
    260   19  562
    592  362  648
    661  292  897
    247  222  349
    947  306   89
    126  329  366
    857  466  329
    594  744  791
    546  202  429
    526  619  501
    54  693  748
    138  682  753
    182  283   74
    613  333  444
    9  403  359
    618  160  457
    312  162  510
    536  506  191
    309  375  567
    72  548  736
    678  983   50
    638  990  699
    263  204  770
    362   13  268
    603  214  866
    564  522   46
    864  918  863
    307  863  842
    364  940  152
    827  403  778
    960  825  792
    13  811  777
    890  912  391
    880  384  443
    735  649  473
    937  443  635
    702  768  256
    604  809  720
    407   56  908
    422  823  198
    376  523  140
    906  236  927
    851  473  978
    449  835  308
    800  156  332
    384  969   62
    282  338  353
    868  472  884
    681  749  966
    744  430  279
    137  425    4
    631  184  285
    781  850  209
    450  376  696
    698  833  851
    307  749  415
    952  124  766
    842  847  658
    95   49  635
    578  934  703
    491  903  253
    721  679   85
    801  363  502
    501  622  421
    657   98  602
    745  979  905
    925  884  326
    88  601  144
    379  699  426
    319  379  293
    564  328  773
    579  716  455
    331  478  990
    789  304  216
    387  466  567
    942  206  417
    840  321   46
    128  457  227
    819  597  243
    666  160  978
    226  513   80
    454  502  927
    860  578  640
    983  130  721
    263  540  201
    591  692  499
    569  939  531
    87  814  872
    753  650  280
    265  715  441
    988  141  611
    746  238  590
    789  735  639
    147  552  139
    37  954  595
    342  411  696
    330  629  474
    314  736  149
    596  684   86
    772  248  136
    442  384  616
    563  153  418
    498  299  818
    408  375  746
    446  394  745
    177   17  534
    257  630  592
    619  878  657
    612  711  637
    594  980  637
    399   57  594
    829  952  547
    286  730  148
    828  225  934
    671  639  963
    249  393  818
    343  531  687
    229  775  270
    987   44  875
    896  666  346
    936  638  753
    299  838  549
    824  735   59
    766  277  572
    582  151  777
    610  144  505
    120   76  902
    300  882  441
    677  435  161
    631  843  567
    62  401  375
    263  744  375
    310  463  141
    108  479  719
    484  266   17
    382  247  712
    158  558  705
    48  479  686
    209  339  958
    20  212  453
    858  254  120
    872  430  344
    801  700  786
    991  700  911
    644  213  692
    911  149  433
    752  795  344
    762  862  332
    764   35  500
    391  639  489
    809  601  497
    148  874   83
    552    4  126
    524  880  195
    530  430  179
    497  345  219
    810  694   47
    995  128  999
    73  149  917
    978  234  146
    394  735   35
    483  478  236
    293  707  271
    614  458  753
    44  531  638
    599  893  188
    729  731  863
    693  915  859
    192  205   17
    999  119  541
    943  262  318
    123  305  514
    339  536  246
    585  228  300
    904  330  325
    127  806   45
    366  429  536
    480  801  555
    656  977  669
    611  969    5
    659  123  671
    644  292  524
    557  143  298
    383  202  824
    306  144   97
    427  541   20
    735  490  109
    14  379  582
    418  367  756
    406  740  823
    832  461  125
    36  692  764
    830  497  768
    632  572   31
    996  607  705
    400  451  712
    586  598  235
    806  364  406
    409  841  606
    971  282  437
    162  208  944
    957  174  591
    567  110  844
    412  211  378
    181  113  637
    145  600  443
    359  588  426
    335  707  255
    345  476  975
    864  521  972
    832   78    5
    556  541  905
    389  712  588
    848  412  813
    833  121  316
    744  743  472
    872  837  421
    185  793  219
    995  168   61
    901  837  158
    693  955  245
    232  634  550
    725  502  451
    526  966  477
    589  607  682
    901  366  247
    815  769  769
    828  627  947
    121  664  262
    708  863    3
    522  968  186
    429  808  186
    378  162  364
    398  361   86
    194  235  285
    657  176  376
    526  787  136
    170  696  290
    513  117  970
    617  797  235
    603  889  900
    706  142  594
    291  321   96
    746  388  668
    764  762  718
    567  821  703
    968  421  883
    926  456  320
    952  963  262
    489  785  540
    446  933  970
    361  607  136
    230  682  987
    27  153  528
    437    3  502
    430  153   51
    349  206  892
    162  683  862
    366  878  244
    976  258  195
    827   80  381
    476  246  569
    881  123  222
    724  342  409
    565  421  511
    703  402  344
    704  549  486
    16  311  414
    198  894   65
    904  475   93
    967  688   43
    246   96  527
    195  327  220
    134  355  395
    222  416  988
    541  343   90
    708  109  900
    245  574  287
    979  617  284
    804  889  130
    174  798  507
    66  623  519
    142  276  146
    308  772  509
    28  773   82
    319    8  524
    507  952  927
    471  867  924
    64  535  874
    959  165  236
    475   98  117
    961  201  293
    397  757  621
    311  287  215
    492  602  425
    111   56  243
    606   69  927
    692   41  875
    523  927  982
    410  810  947
    188  120  120
    543  569   92
    838  442  414
    595  789  505
    16  896  639
    642  843  845
    644  116  938
    26  347  466
    376  664  681
    404  946  571
    766  195  436
    427  134  509
    336   85  163
    575  689  670
    598  236  705
    53  498   76
    977   35   38
    674   41  911
    572   37  901
    118  714  700
    499  958  771
    477  841   86
    633  877  670
    776  873  625
    260   88   85
    281  816  296
    568  966  670
    653  267  684
    245   23  935
    316  235  462
    99  246  739
    278  652  240
    367  542  525
    150  246  563
    422  937  236
    57  994  430
    472  188  202
    636  778  684
    756  912  605
    461  769  534
    646  508   56
    963  305  767
    618  686  769
    684  750    1
    567  173  333
    896  805  330
    821  987  638
    805  127  644
    74  899   12
    444  434  269
    449  147  181
    35  542  321
    320  105  163
    815  289  546
    537  208  691
    754  345   54
    116  804  780
    694  512  744
    941  423  795
    936  147  702
    8  327  865
    42  141  761
    189  658  273
    214  545  668
    259  219  322
    455  782  551
    512  646  526
    127  710  932
    169  615  729
    55  482  381
    571   98  930
    362  721  296
    645  684  804
    701  257  529
    731  711  683
    225  575  423
    918   23  492
    917  271  217
    5  293  537
    733  595  105
    741  433  932
    31  259  920
    303  691  396
    363  580  863
    427  474  576
    703  175  246
    529  280  209
    641  412  319
    530  890  726
    382  779  433
    284  147  323
    860  703  766
    292  427  768
    606  666  736
    638  384  264
    97  398  574
    564  627  455
    678  442  618
    432   28  687
    343  466  101
    304  594    5
    402   43  474
    498  552  476
    243  113  538
    938  102  498
    829   44  470
    208  449  197
    352  286  425
    503  600  241
    531  209  490
    615  758  363
    518  783  258
    886  636  579
    386  960  821
    678  547  784
    481  843  238
    476  256    6
    2  973  233
    331  144  822
    410  545  872
    558  546   49
    1  388  106
    384  557  457
    387  699  460
    592  809  571
    388  942  111
    651  756  647
    860  518  794
    722  894  731
    324  881  256
    429  197  383
    226  239  685
    489   51  339
    746  402  745
    756  608  434
    220  452  629
    773  631  798
    296  137  537
    942  704  260
    820  565  673
    745  666  110
    411  406  721
    44  522  531
    777  394  494
    807  835   66
    36  362   38
    113  290  975
    141  124  953
    38  663   51
    507  677  316
    514  115  360
    956  194  865
    642  677  111
    682  868  889
    173  507  339
    270  546  318
    279  115  177
    803  915  913
    139  107  187
    925  995  943
    467  471  233
    433  321  977
    689  358  961
    544  479  788
    450  403  438
    868  155  740
    583  251  384
    163  220  215
    732   43  519
    253  323  234
    481  314  873
    298  284  975
    574  480  690
    357  181   86
    292  627  695
    817  908  950
    302  137  865
    553  991  507
    660  645  933
    752  819  831
    437  189  346
    144  283  738
    108  491  429
    91  209  688
    289   24  138
    676  604   82
    670  585  204
    78  439  403
    81  674  548
    65  476  475
    904  502  542
    873  723  506
    909  628  337
    918  227  277
    929  208  324
    255  125  477
    415   30  618
    981   24  546
    588   38  694
    876  102  663
    628  262  927
    419  277  964
    160  523  632
    424  250  582
    452  773   68
    466  162  225
    913   64  463
    486   95  612
    350  264  427
    723  184  872
    868  376  701
    441  318  509
    506  286   58
    528  448  513
    166  533   92
    169  552  414
    298   85  320
    663  883  192
    664  572  433
    12  738  538
    330  943   94
    398  814  754
    273  195  682
    980  461  544
    282  558  554
    788  117  660
    336  326  337
    465  184  829
    352  180  988
    130  707  122
    854  295  141
    943  444  176
    14    4  473
    514   11  603
    525   10  892
    22  700  427
    400  673  470
    411  288  583
    448  431  582
    466  752  501
    722  530  104
    509  673  464
    727  112  677
    567  614  716
    677    9  561
    620  976  603
    324  965   87
    215  738  844
    206  128  637
    91  727  427
    625  343  624
    639  770  757
    859  691  710
    278  140  634
    405  798  561
    356  903   78
    54  748  890
    858  870  310
    821  723  983
    385  489  431
    776  261  962
    834  527  752
    798  945  282
    802  788  288
    73  899  228
    689  667   74
    995  691  603
    656  320  652
    841   63   62
    176  797   83
    736  788   20
    558  775  454
    275  858  544
    792  353   95
    494  939  694
    126  964  408
    388   65  952
    229  368  992
    408  180  481
    618  347  793
    860    4  641
    49  296  895
    901  294  563
    24  434  159
    368  911  593
    380  864  492
    637  378  275
    614  383  295
    246  326  514
    373  617  382
    409  214  294
    263  802  419
    820  909  683
    136  452  290
    860  558  491
    789  235  334
    116  508   41
    721  691  303
    14  176   77
    217  281  289
    231  337  237
    348  400  206
    382  317  684
    33  187  667
    467  434   77
    534  588  846
    905  483  871
    810  422   10
    724  260  181
    155  259  189
    223  860  544
    271  706  576
    109  431  554
    904  486  465
    65  317  535
    837  365  126
    496  700  349
    642  252  496
    750  706  505
    956  978  875
    845  953   89
    138  259  805
    787  793  322
    59  739  128
    790  957  389
    126  232  160
    391  807  909
    454  814  762
    532  358  272
    81  718  554
    482  413  655
    185  141  894
    734   96  739
    583  204  372
    587  314  772
    402  496  292
    798  187  543
    516  824  922
    409  146  140
    408  922  840
    50  373  244
    355  476  321
    365  181  219
    601  842  642
    426  426  889
    482  828  584
    589  698  588
    835   97  287
    759  769  768
    858  520  481
    947  517  219
    266  101  400
    110   65  448
    920  310  431
    926  318   23
    388  698  374
    45  753  406
    421   68  374
    203  268  623
    345  586  611
    344  678   32
    542  830  187
    487  916  456
    211  905  567
    760   36  585
    327  380  425
    793  405  604
    432  630  267
    804  307  366
    678  716  108
    437  316   22
    441  214  160
    329  364  149
    726   82   76
    896  857  361
    298  796  294
    652  406  409
    855  123  704
    430  461  744
    909  130   88
    847  910  968
    499  898  952
    662  458   83
    335  589  395
    863  481  361
    829  217  305
    822  306  635
    664  212  610
    271  203  675
    95  409  236
    337  408  517
    749  357  294
    646  214  322
    457  181  157
    649  391   73
    756  258   84
    201  312   11
    660   14  560
    128  491   96
    676  507  570
    302  944   54
    25   48  296
    313  916  351
    932  496  355
    437  198  217
    826  627  370
    312  259  193
    293  718  654
    142  459  647
    233  336   90
    81  360  232
    168   23  144
    442  523  662
    151  952  260
    444  963  489
    302  934  249
    687  523  698
    672  494  462
    461  198  649
    540  410  269
    539  515  839
    76  494   81
    514  571  496
    438  216  557
    279  215  690
    581  362  667
    318  229   53
    427  590   52
    510   61  485
    168  596  451
    960   61  155
    475   32  291
    933   57  182
    458  702  818
    740   29  821
    896  716  738
    842  614  701
    398  466  502
    739  612  737
    297  349  887
    262  303  886
    404  302  428
    668  758  600
    674  815  379
    44  395  225
    673  341  476
    516  403  384
    392  168  134
    907  562  511
    555  406  413
    818  737  773
    745  588  545
    323   27  639
    555  571  802
    326  712  513
    729  310  664
    810  468  804
    509   79  438
    418  680  403
    314  600  470
    186  678   12
    302  381  921
    380  946  932
    104  725  596
    113  950  471
    11  478  291
    520  535  519
    574  113  668
    862  541  617
    606  173  314
    960  141  857
    837  154  926
    933  161  137
    255   86  277
    794  117  308
    664  305  588
    321  329  554
    961   22  181
    433  604  132
    655  546  431
    553  187  373
    797  526  674
    563  122  203
    352  613  626
    780  196  549
    253  191  547
    746   15  412
    193  523  391
    281  537  657
    90   99  658
    892  737  646
    503  717  663
    693  205   50
    700  409  662
    465  896   15
    865  901  661
    508  664  192
    770   36  898
    980  638  809
    301  307  626
    724  803  839
    851  514  256
    521  225  170
    678  217  789
    735  289  904
    501  745  719
    896  434  575
    819  850  977
    607  691  590
    28  840  476
    623  647  167
    890  740  708
    465  674  219
    753  127  674
    370  149  657
    189  802  476
    455  734  376
    37  988  149
    947  966  168
    921   20  311
    798  911  338
    768  467  681
    102  554  488
    747  178  608
    747  507  690
    372  490  197
    22  974  389
    717  173  396
    734  881  363
    560  889  821
    721   54  963
    164  836  833
    135  687  910
    204  995  188
    188  623  865
    877  495  264
    335  316  399
    593  234  300
    655  418  169
    471  551   86
    444  381  226
    820  461  304
    38   90  334
    845  471   47
    449  124  218
    694  134  547
    270   31  551
    434  195  750
    774  171  789
    877   92  222
    843  153  340
    758  184  326
    252  239  372
    651  915  540
    740  369   10
    828  998  547
    814  903  348
    491  364  548
    549  612  267
    429  170  705
    978  231  210
    917  339  866
    432  133  494
    983  409  450
    567  465  246
    744  885  684
    717  876  790
    183   11  138
    984  740  311
    686  528  843
    757  343  712
    31  752  940
    17  254  106
    11  956  974
    495  572  789
    440  496  763
    183  254   25
    152  559  980
    146  573  585
    200  456  938
    371  716  716
    228  742  679
    205  167  988
    81  223  806
    509  421  192
    544  485  828
    112  205  502
    45  133  555
    155  134  784
    179  196  625
    81   18  589
    165  187  754
    63  341  774
    197  266  913
    229  306  680
    509  317  787
    976  260  535
    661  228  518
    289  398  700
    173  398  645
    170  370  283
    574  346  446
    422  511   42
    275  305  482
    118  698  869
    338  616  897
    334  520  229
    461  570  136
    84  336  245
    531  242  157
    646  786  356
    372  841  124
    701   67  294
    481  880  626
    155  951  361
    365  273  946
    555  163   93
    163   11  515
    582  168  495
    128  732  752
    489  685  558
    529  788  341
    820  489   73
    941  118  684
    188  545  680
    64  923  188
    639  859  445
    656  985  606
    769  169    9
    931  310  472
    205  177  472
    702   72   27
    490  977  472
    914  972  495
    837  417  333
    437  505  440
    947  881  490
    379  998  906
    371  789  763
    604  804  200
    250  622  969
    778  445  924
    844  925   78
    346  843  440
    409  228  396
    73  902  717
    759   75  511
    720  172  435
    427  155  938
    75  920  213
    49  895  187
    122   33  211
    216  875  709
    169  644  534
    47  243  546
    942  591  200
    874  790  870
    667  478  797
    475  900  250
    92  442  709
    386  505  856
    70  747  595
    390  854  740
    333  267  730
    576  421  336
    872  428  352
    884  676   20
    119  686    6
    126  171  145
    6  548  140
    296  406  627
    276  546  653
    329  232  356
    811  203  648
    757  762  656
    117  717   71
    718   17  608
    965  859  794
    707  870  536
    251  549  245
    259  814  314
    498  510  247
    885  287  649
    339  312  690
    839  518  589
    691  875  327
    549  852  692
    421  764  399
    841  938  617
    683   73  682
    838  903  513
    155  700  266
    529  492  436
    501  209  572
    177  405  185
    70  407   98
    183  151  184
    144    4  719
    387  261  642
    391  259  283
    336  371  946
    902  810  751
    714  509  251
    124  194  440
    928  357  256
    921  241  599
    725  694  141
    219  281  864
    650  757  979
    515  678  735
    553  766  397
    771  749  360
    273  414  543
    92  185  692
    365  480  246
    153  416  246
    527  431  831
    387  385  991
    610  849   42
    761  473  507
    268  863  467
    797  777  614
    567  810  153
    384   43  459
    656   78   14
    884  328   89
    565  340   99
    315  431  403
    169  687  408
    451  365    7
    706  807  561
    324  769  821
    645  257  467
    90  654  617
    230  859  466
    309  363  691
    903  656  499
    165  880  548
    966  474   64
    640  851  817
    585  967  754
    340  699  880
    253  575   32
    783  349  971
    748  532  972
    132  312  789
    673  254  152
    769   90  643
    485  483    3
    228  254  423
    281  282  418
    279  536  891
    566  876  988
    753  959  569
    629  379  773
    194  468  866
    690  307  128
    813  568  374
    798  387  873
    980  363  555
    337  750  620
    565  783  190
    780   32  746
    716  222  594
    319   50  443
    498  212  408
    106  552   95
    96  465  866
    143  216  903
    570  209  371
    73  136  163
    579  290  400
    836  640  252
    190   81  864
    684  657  892
    405  876  982
    722  499  926
    567  764   62
    383  573  458
    111  337  947
    381  366  568
    883  323  560
    942  136  297
    103  324  576".

day_4a_input() ->
  "nzydfxpc-rclop-qwzhpc-qtylyntyr-769[oshgk]
qzlozfhmf-bzmcx-bnzshmf-zbpthrhshnm-339[zmxdi]
xtwtelcj-rclop-upwwjmply-zapcletzyd-743[itbds]
mrxivrexmsrep-fewoix-ywiv-xiwxmrk-308[kzypw]
ibghopzs-qobrm-difqvogwbu-142[lnrzo]
irgyyolokj-xghhoz-lotgtiotm-228[vnmxd]
foadouwbu-gqojsbusf-vibh-fsgsofqv-376[ymswi]
jvuzbtly-nyhkl-wyvqljapsl-wshzapj-nyhzz-ylzlhyjo-903[jnyaz]
amjmpdsj-cee-qcptgacq-236[caejm]
nvrgfezqvu-avccpsvre-crsfirkfip-217[nrzcd]
vdzonmhydc-bzmcx-bnzshmf-zbpthrhshnm-235[znyot]
oxaflxzqfsb-mixpqfz-doxpp-abmxoqjbkq-393[xqbfo]
drxevkzt-upv-crsfirkfip-893[rfikp]
rkpqxyib-zelzlixqb-obzbfsfkd-627[wzymt]
ajvyjprwp-kdwwh-bjunb-563[jwbpa]
nzcczdtgp-eza-dpncpe-mldvpe-opalcexpye-171[wjzyc]
jxdkbqfz-zixppfcfba-mixpqfz-doxpp-abmilvjbkq-809[nbaoc]
tbxmlkfwba-zelzlixqb-xkxivpfp-315[yixla]
gvcskirmg-gerhc-gsexmrk-hizipstqirx-620[zyosg]
ygcrqpkbgf-ecpfa-eqcvkpi-uvqtcig-440[sbpfj]
irdgrxzex-vxx-rthlzjzkzfe-425[zdqok]
szfyrqriuflj-tfcfiwlc-wcfnvi-uvjzxe-425[lfipb]
zgmfyxypbmsq-aylbw-bcqgel-912[dqtlr]
ubhatstkwhnl-cxeeruxtg-wxitkmfxgm-475[ectpi]
ejpanjwpekjwh-fahhuxawj-wymqeoepekj-316[utnvq]
qlm-pbzobq-ciltbo-pqloxdb-601[lktqh]
votubcmf-fhh-efwfmpqnfou-493[aiyzx]
jqwpihizlwca-uqtqbizg-ozilm-kivlg-twoqabqka-382[frmkh]
zovldbkfz-bdd-tlohpelm-497[dcfxw]
atyzghrk-yigbktmkx-natz-jkyomt-748[ktyag]
iuxxuyobk-hgyqkz-iayzuskx-ykxboik-566[yzsxi]
jvsvymbs-zjhclunly-obua-klzpnu-903[ckmyr]
sbnqbhjoh-ezf-tbmft-623[bfhte]
forwcoqhwjs-rms-difqvogwbu-870[owfqr]
yhtwhnpun-ibuuf-zopwwpun-721[tjlfz]
guahyncw-wbiwifuny-omyl-nymncha-968[nywac]
ykjoqian-cnwza-bhksan-zaoecj-576[btksq]
ckgvutofkj-igtje-zxgototm-800[mlqgz]
ipvohghykvbz-jvsvymbs-lnn-dvyrzovw-227[pysto]
jqwpihizlwca-zilqwikbqdm-ntwemz-kwvbiqvumvb-382[antsy]
njmjubsz-hsbef-cvooz-mbcpsbupsz-805[bferv]
ojk-nzxmzo-xgvnndadzy-xcjxjgvoz-mzxzdqdib-785[vzpun]
yhkpvhjapcl-ihzrla-svnpzapjz-747[pahzj]
htqtwkzq-zsxyfgqj-kqtbjw-rfsfljrjsy-827[wzxif]
fubrjhqlf-sodvwlf-judvv-ilqdqflqj-257[flqdj]
forwcoqhwjs-qobrm-qcohwbu-rsgwub-116[gtsir]
nzydfxpc-rclop-mfyyj-cpdplcns-483[gzysm]
cjpibabsepvt-cbtlfu-nbobhfnfou-857[bfcno]
oxmeeuruqp-ngzzk-geqd-fqefuzs-222[equzf]
mvkccspson-lexxi-bomosfsxq-614[ybgsn]
frqvxphu-judgh-fdqgb-frdwlqj-vhuylfhv-231[bojwc]
sno-rdbqds-bgnbnkzsd-btrsnldq-rdquhbd-989[dbnsq]
wrs-vhfuhw-fdqgb-frdwlqj-uhvhdufk-179[fhduw]
ftzgxmbv-ynssr-vahvhetmx-mktbgbgz-397[truqi]
xst-wigvix-tpewxmg-kveww-ywiv-xiwxmrk-984[nmrid]
dkqjcbctfqwu-rncuvke-itcuu-ucngu-544[inaxl]
hwdtljsnh-wfggny-fsfqdxnx-151[anbvm]
aflwjfslagfsd-bwddqtwsf-vwhdgqewfl-970[sjqtc]
hmsdqmzshnmzk-okzrshb-fqzrr-sqzhmhmf-885[qzbly]
xcitgcpixdcpa-qphzti-gtrtxkxcv-141[ctxip]
nbhofujd-tdbwfohfs-ivou-gjobodjoh-649[fnwgk]
wfruflnsl-wfggny-xjwanhjx-905[lnztx]
jrncbavmrq-rtt-fgbentr-975[rtbna]
jqwpihizlwca-akidmvomz-pcvb-bziqvqvo-460[zakpd]
oxmeeuruqp-nmewqf-eqdhuoqe-560[vtznk]
plolwdub-judgh-udeelw-uhvhdufk-985[fnsty]
kfg-jvtivk-treup-ivrthlzjzkzfe-503[ktvze]
bqvvu-ywjzu-ykwpejc-naoawnyd-550[txaws]
gsvvswmzi-wgezirkiv-lyrx-qerekiqirx-984[irevg]
vcibutulxiom-yaa-uhufsmcm-552[gpimb]
ugjjgkanw-usfvq-ugslafy-lwuzfgdgyq-918[gufaj]
qfmcusbwq-foadouwbu-dzoghwq-ufogg-hsqvbczcum-506[uoqbc]
qzchnzbshud-rbzudmfdq-gtms-zmzkxrhr-547[gxjyh]
aflwjfslagfsd-usfvq-ugslafy-esjcwlafy-450[kjnil]
nzwzcqfw-nsznzwlep-afcnsldtyr-951[hjrdi]
aoubshwq-pogysh-kcfygvcd-740[pxyzt]
zbytomdsvo-lkcuod-nocsqx-380[rqsuo]
fmsledevhsyw-jpsaiv-wivzmgiw-542[pitok]
qvbmzvibqwvit-ntwemz-abwziom-304[urjat]
gvaaz-tdbwfohfs-ivou-eftjho-389[inlud]
wlqqp-avccpsvre-ivjvrity-399[qcgto]
shmml-pelbtravp-pubpbyngr-znantrzrag-897[prabn]
iruzfrtkzmv-jtrmvexvi-ylek-jvimztvj-477[vijmr]
hdgdovmt-bmvyz-xjgjmapg-xviyt-xjvodib-yzqzgjkhzio-499[tsira]
willimcpy-yaa-omyl-nymncha-916[jtwau]
dpssptjwf-gmpxfs-bobmztjt-389[kjlin]
gpbepvxcv-snt-tcvxcttgxcv-245[etyfm]
jxdkbqfz-ciltbo-cfkxkzfkd-367[xldij]
udglrdfwlyh-sodvwlf-judvv-pdunhwlqj-231[rslnt]
ygcrqpkbgf-ecpfa-eqcvkpi-gpikpggtkpi-440[pgkci]
sno-rdbqds-eknvdq-otqbgzrhmf-391[yzexr]
lzfmdshb-atmmx-bnmszhmldms-313[bacsl]
qczcftiz-xszzmpsob-aofyshwbu-974[nxzyq]
nwzekwypera-fahhuxawj-iwngapejc-420[yzguv]
sbnqbhjoh-gmpxfs-efwfmpqnfou-389[sbwne]
ugjjgkanw-xdgowj-ksdwk-554[sntqw]
wdjcvuvmyjpn-kgvnodx-bmvnn-ncdkkdib-213[apifx]
nvrgfezqvu-tyftfcrkv-uvgcfpdvek-269[tiuvc]
uiovmbqk-jcvvg-abwziom-252[trvgn]
fydelmwp-prr-ecltytyr-561[ryelp]
ujoon-rpcsn-rdpixcv-jhtg-ithixcv-999[dcbon]
ahngzyzqcntr-eknvdq-dmfhmddqhmf-131[dhmnq]
iqmbazulqp-ngzzk-fqotzaxask-222[nmtgf]
tfejldvi-xiruv-avccpsvre-nfibjyfg-113[vfice]
wlsiayhcw-nij-mywlyn-xsy-nywbhifias-552[otlcf]
muqfedyput-sqdto-seqjydw-skijecuh-iuhlysu-764[mqlfy]
wbhsfbohwcboz-suu-hfowbwbu-324[bwhou]
owshgfarwv-hdsklau-yjskk-suimakalagf-944[wtiza]
ahngzyzqcntr-rbzudmfdq-gtms-bnmszhmldms-859[yvztm]
qyujihctyx-vohhs-fiacmncwm-292[knzep]
vetllbybxw-xzz-ybgtgvbgz-579[bgzlt]
zekvierkzferc-tyftfcrkv-uvgrikdvek-477[pecqo]
molgbzqfib-mixpqfz-doxpp-obzbfsfkd-367[ptnjg]
oxmeeuruqp-otaoaxmfq-geqd-fqefuzs-924[puwxb]
gpewwmjmih-nippcfier-gsrxemrqirx-646[tpsoa]
xgvnndadzy-wvnfzo-gvwjmvojmt-603[nfstl]
bjfutsneji-hfsid-htfynsl-htsyfnsrjsy-983[fdzej]
pynffvsvrq-wryylorna-qrcnegzrag-351[dymsz]
xjgjmapg-rzvkjiduzy-wvnfzo-kpmxcvndib-109[eyrcm]
sebehvkb-rkddo-vydqdsydw-296[lgonm]
hjgbwuladw-uzgugdslw-kwjnauwk-138[kgloe]
ohmnuvfy-wuhxs-wiuncha-uwkocmcncih-890[tvylz]
udpsdjlqj-iorzhu-wudlqlqj-491[rtwma]
rdggdhxkt-ytaanqtpc-ejgrwphxcv-635[gtacd]
eqttqukxg-ecpfa-yqtmujqr-596[hnvwy]
laffe-lruckx-iutzgotsktz-852[nglsy]
pkl-oaynap-xwogap-yqopkian-oanreya-394[ebzft]
kyelcrga-afmamjyrc-bcnyprkclr-314[crayk]
joufsobujpobm-dipdpmbuf-ufdiopmphz-571[ytgxs]
ydjuhdqjyedqb-fbqijys-whqii-tufqhjcudj-894[ekswi]
ajvyjprwp-kdwwh-mnenuxyvnwc-251[psaok]
ibghopzs-suu-gsfjwqsg-220[qshwm]
ugjjgkanw-hdsklau-yjskk-dstgjslgjq-502[lqxth]
ugdgjxmd-hdsklau-yjskk-hmjuzskafy-788[vtyzi]
lqwhuqdwlrqdo-mhoobehdq-oderudwrub-751[bzype]
wdjcvuvmyjpn-kgvnodx-bmvnn-pnzm-oznodib-837[dcbaw]
vcibutulxiom-qyujihctyx-wbiwifuny-fiacmncwm-994[tjxnm]
vqr-ugetgv-dcumgv-tgegkxkpi-102[gvekt]
encuukhkgf-lgnnadgcp-wugt-vguvkpi-752[vdmps]
ktfitzbgz-wrx-vhgmtbgfxgm-267[twjsh]
dzczkrip-xiruv-jtrmvexvi-ylek-kvtyefcfxp-243[vctis]
pynffvsvrq-enzcntvat-qlr-qrcnegzrag-377[vdxzy]
zixppfcfba-mixpqfz-doxpp-ixyloxqlov-679[pxfio]
shoewudys-isqludwuh-xkdj-kiuh-juijydw-608[qedlg]
qjopwxha-ywjzu-ykwpejc-nayaerejc-524[syznu]
upq-tfdsfu-dboez-dpbujoh-nbobhfnfou-415[bfoud]
fruurvlyh-udeelw-dqdobvlv-907[psznj]
tcfkqcevkxg-dwppa-wugt-vguvkpi-336[klouj]
plolwdub-judgh-gbh-uhdftxlvlwlrq-621[bnwav]
chnylhuncihuf-wuhxs-wiuncha-xypyfijgyhn-110[hnuyc]
froruixo-gbh-rshudwlrqv-153[amoni]
clxalrtyr-nsznzwlep-zapcletzyd-327[guevs]
xgsvgmotm-igtje-rghuxgzuxe-306[lcxzy]
gpsxdprixkt-rpcsn-gtprfjxhxixdc-401[xprcd]
lsyrkjkbnyec-nio-bokmaescsdsyx-874[nkgyo]
buzahisl-ibuuf-zopwwpun-747[vwuxt]
vqr-ugetgv-fag-yqtmujqr-882[gqrtu]
bgmxkgtmbhgte-wrx-wxlbzg-137[zquts]
ujqgywfau-ugdgjxmd-tskcwl-hmjuzskafy-112[yolzm]
lqwhuqdwlrqdo-fkrfrodwh-frqwdlqphqw-985[qwdrf]
surmhfwloh-fdqgb-uhvhdufk-621[dimhn]
fhezusjybu-hqrryj-qdqboiyi-322[esjtk]
gvaaz-tdbwfohfs-ivou-tijqqjoh-857[oafhi]
xmtjbzidx-xviyt-mznzvmxc-759[xmzit]
tfiifjzmv-gcrjkzt-xirjj-dribvkzex-399[ijrzf]
zuv-ykixkz-kmm-uvkxgzouty-748[xvipz]
gokzyxsjon-cmkfoxqob-rexd-domrxyvyqi-172[yfozw]
plolwdub-judgh-frqvxphu-judgh-sodvwlf-judvv-frqwdlqphqw-959[duhlq]
kfg-jvtivk-treup-tfekrzedvek-373[xyqzr]
dmybmsuzs-oaddaeuhq-qss-pqeusz-586[mansh]
ryexqpqhteki-rqiauj-iqbui-660[trqbl]
ytu-xjhwjy-jll-wjhjnansl-125[zketn]
fhezusjybu-muqfedyput-sqdto-udwyduuhydw-660[udyef]
qspkfdujmf-dipdpmbuf-mphjtujdt-909[dwuov]
cebwrpgvyr-enqvbnpgvir-enoovg-nanylfvf-533[akmyb]
qfmcusbwq-pogysh-hfowbwbu-246[hdsbg]
dmybmsuzs-bxmefuo-sdmee-abqdmfuaze-846[wsntq]
sno-rdbqds-azrjds-nodqzshnmr-157[ogtbm]
dpotvnfs-hsbef-cbtlfu-qvsdibtjoh-493[isjkl]
nwilwcejc-lhwopey-cnwoo-odellejc-732[isofe]
gokzyxsjon-mkxni-mykdsxq-bomosfsxq-822[nyudi]
molgbzqfib-gbiivybxk-obxznrfpfqflk-679[wotra]
cybyjqho-whqtu-rqiauj-jhqydydw-920[qyhjd]
tinnm-pogysh-ghcfous-506[xtcso]
myvybpev-cmkfoxqob-rexd-cdybkqo-146[boycd]
pbeebfvir-shmml-enoovg-chepunfvat-949[nltsr]
apuut-ezggtwzvi-yzqzgjkhzio-135[gdywb]
xfbqpojafe-ezf-bobmztjt-441[qbwdc]
ktfitzbgz-xzz-ehzblmbvl-943[zdigt]
eqnqthwn-dcumgv-rwtejcukpi-232[umdca]
xlrypetn-clmmte-opawzjxpye-379[bkvqj]
fydelmwp-awldetn-rcldd-lylwjdtd-821[dlwet]
owshgfarwv-esyfwlau-kusnwfywj-zmfl-ugflsafewfl-658[fwlsa]
bnmrtldq-fqzcd-eknvdq-sqzhmhmf-807[snkmf]
tyepcyletzylw-upwwjmply-cpnptgtyr-951[kyfol]
ckgvutofkj-igtje-iugzotm-ynovvotm-462[otgvi]
jshzzpmplk-ihzrla-klzpnu-981[yiaum]
aoubshwq-qczcftiz-qvcqczohs-qighcasf-gsfjwqs-896[hyxzq]
zilqwikbqdm-kpwkwtibm-ewzsapwx-668[zujgy]
tpspahyf-nyhkl-jhukf-jvhapun-zopwwpun-669[slmnt]
iuruxlar-lruckx-aykx-zkyzotm-852[kruxa]
bdavqofuxq-bxmefuo-sdmee-dqeqmdot-144[gblym]
eqnqthwn-ygcrqpkbgf-dcumgv-octmgvkpi-778[aznbf]
amppmqgtc-kyelcrga-bwc-ylyjwqgq-522[aedsm]
diozmivodjivg-xviyt-xjvodib-vxlpdndodji-109[dcnaq]
wifilzof-wbiwifuny-uwkocmcncih-474[lcymu]
dpmpsgvm-dboez-dpbujoh-efwfmpqnfou-779[ujlre]
iwcjapey-fahhuxawj-nayaerejc-758[elvba]
tcorcikpi-ejqeqncvg-vgejpqnqia-414[tywiv]
excdklvo-mrymyvkdo-wkbuodsxq-120[ngxcw]
nbhofujd-qmbtujd-hsbtt-bobmztjt-467[evdab]
fubrjhqlf-gbh-pdqdjhphqw-959[nmklz]
lxuxaodu-snuuhknjw-cnlqwxuxph-667[skyjn]
ytu-xjhwjy-jll-zxjw-yjxynsl-931[tyzfu]
cjpibabsepvt-sbccju-tbmft-519[ijnyz]
bkwzkqsxq-lexxi-cobfsmoc-406[xbcko]
yaxsnlcrun-dwbcjkun-kdwwh-orwjwlrwp-823[wnrcd]
jchipqat-gpqqxi-ejgrwphxcv-739[pqcgh]
etyyx-dff-cdudknoldms-937[dfyce]
jsehsyafy-vqw-dgyaklauk-112[rhsgp]
nzwzcqfw-awldetn-rcldd-opalcexpye-587[cdelw]
nvrgfezqvu-wcfnvi-uvjzxe-139[vefnu]
apuut-kgvnodx-bmvnn-ncdkkdib-915[stofz]
hwdtljsnh-ojqqdgjfs-zxjw-yjxynsl-489[ftzsy]
pualyuhapvuhs-jhukf-jvhapun-ayhpupun-877[oflqz]
yaxsnlcrun-bljenwpna-qdwc-mnenuxyvnwc-407[ncwae]
eqnqthwn-ejqeqncvg-nqikuvkeu-102[exsvc]
ynssr-lvtoxgzxk-angm-tvjnblbmbhg-813[bgnlm]
sehheiylu-zubboruqd-cqdqwucudj-400[ziuys]
awzwhofm-ufors-qobrm-gozsg-246[zurge]
ahngzyzqcntr-bzmcx-bnzshmf-lzmzfdldms-677[zmnbc]
eza-dpncpe-awldetn-rcldd-wzrtdetnd-769[mtgef]
htsxzrjw-lwfij-kqtbjw-wjxjfwhm-931[sjxwa]
szfyrqriuflj-jtrmvexvi-ylek-nfibjyfg-373[alfvj]
emixwvqhml-ntwemz-amzdqkma-876[lutzf]
hwbba-fag-tgegkxkpi-726[zilbc]
nwilwcejc-zua-zalhkuiajp-212[acijl]
aflwjfslagfsd-esyfwlau-kusnwfywj-zmfl-mkwj-lwklafy-892[flwas]
fmsledevhsyw-fewoix-xiglrspskc-256[sefil]
zuv-ykixkz-lruckx-ykxboiky-306[odviu]
ftzgxmbv-utldxm-ftgtzxfxgm-969[noxmk]
ojk-nzxmzo-nxvqzibzm-cpio-mzvxlpdndodji-109[nfysr]
molgbzqfib-mixpqfz-doxpp-abmilvjbkq-133[bimpq]
zekvierkzferc-tyftfcrkv-jkfirxv-867[bjyft]
gvaaz-ezf-bobmztjt-727[muyvq]
mvhkvbdib-zbb-hvivbzhzio-551[yntvz]
dwbcjkun-kdwwh-ujkxajcxah-641[jkwac]
lhkhszqx-fqzcd-qzaahs-lzmzfdldms-209[tklbi]
njmjubsz-hsbef-sbccju-qvsdibtjoh-571[tpswx]
avw-zljyla-lnn-zopwwpun-149[lnwap]
ckgvutofkj-hgyqkz-iayzuskx-ykxboik-358[mdnay]
ytu-xjhwjy-hfsid-rfsfljrjsy-697[hmiag]
houngfgxjuay-igtje-sgtgmksktz-384[monuj]
qfkkj-ojp-ecltytyr-249[itsvz]
udglrdfwlyh-udeelw-zrunvkrs-985[dlrue]
gsvvswmzi-tpewxmg-kveww-viwievgl-178[vbmit]
houngfgxjuay-yigbktmkx-natz-ykxboiky-930[bjyzu]
eqttqukxg-uecxgpigt-jwpv-uvqtcig-102[gnzpx]
amjmpdsj-bwc-pcqcypaf-288[gsywq]
zntargvp-cynfgvp-tenff-npdhvfvgvba-793[vfngp]
hdgdovmt-bmvyz-xcjxjgvoz-ozxcijgjbt-811[snjmz]
etaqigpke-rncuvke-itcuu-ewuvqogt-ugtxkeg-154[eugtk]
pbybeshy-sybjre-ybtvfgvpf-117[vszqj]
vqr-ugetgv-tcddkv-uvqtcig-336[hzkdw]
ykhknbqh-xwogap-pnwejejc-368[nqxzy]
sehheiylu-rkddo-jusxdebewo-634[edhos]
mvydjvxodqz-xcjxjgvoz-adivixdib-395[snpmt]
ahngzyzqcntr-bzmcx-rsnqzfd-183[zfkpc]
qxdwpopgsdjh-eaphixr-vgphh-detgpixdch-973[hpdgx]
nij-mywlyn-jlidywncfy-jfumncw-alumm-zchuhwcha-682[nvybs]
yhtwhnpun-jhukf-klzpnu-383[hnukp]
yhtwhnpun-qlssfilhu-svnpzapjz-175[fopst]
sgmtkzoi-jek-jkbkruvsktz-774[whzny]
gifavtkzcv-wcfnvi-rercpjzj-711[cvfij]
pkl-oaynap-acc-lqnydwoejc-940[aclno]
vhkkhlbox-ktuubm-tvjnblbmbhg-475[gyhzn]
guahyncw-wfummczcyx-wuhxs-guleyncha-422[iybfa]
ydjuhdqjyedqb-hqrryj-sedjqydcudj-816[ftnej]
rnqnyfwd-lwfij-gfxpjy-xytwflj-801[phsxc]
vdzonmhydc-qzaahs-rdquhbdr-365[quvjr]
ucynmlgxcb-zyqicr-rcaflmjmew-444[ncmzr]
wfummczcyx-gcfcnuls-aluxy-luvvcn-jolwbumcha-864[culma]
qzoggwtwsr-dfcxsqhwzs-suu-qighcasf-gsfjwqs-220[sgqwf]
esyfwlau-mfklstdw-uzgugdslw-suimakalagf-918[igapw]
zntargvp-wryylorna-genvavat-819[zmnji]
ugfkmewj-yjsvw-uzgugdslw-umklgewj-kwjnauw-268[wugjk]
xjinphzm-bmvyz-kgvnodx-bmvnn-pnzm-oznodib-109[rfzpw]
mvydjvxodqz-mvwwdo-ozxcijgjbt-265[cxfdz]
ujoon-rwdrdapit-bpcpvtbtci-375[ptbcd]
aczupnetwp-dnlgpyrpc-sfye-afcnsldtyr-457[cydjq]
uzfqdzmfuazmx-eomhqzsqd-tgzf-ymzmsqyqzf-898[zmqfd]
lugjuacha-luvvcn-ijyluncihm-890[uclah]
mtzslklcozfd-upwwjmply-lnbftdtetzy-379[yzwvu]
myvybpev-oqq-bocokbmr-328[bomqv]
tagzsrsjvgmk-xdgowj-ghwjslagfk-346[anbri]
dpmpsgvm-ezf-mphjtujdt-493[dolas]
votubcmf-sbccju-efqbsunfou-831[mnvky]
zotts-xsy-nywbhifias-188[boqzr]
gvcskirmg-glsgspexi-irkmriivmrk-646[igrkm]
bnknqetk-cxd-cdozqsldms-625[dcknq]
eqnqthwn-lgnnadgcp-cpcnauku-206[ytmnv]
kmjezxodgz-wpiit-mznzvmxc-525[gtdob]
dpotvnfs-hsbef-fhh-dvtupnfs-tfswjdf-519[fsdht]
bwx-amkzmb-kpwkwtibm-lmdmtwxumvb-486[qmvwa]
guahyncw-vohhs-lyuwkocmcncih-760[chnou]
guahyncw-zfiqyl-nywbhifias-188[szhfp]
ksodcbwnsr-foppwh-kcfygvcd-194[cdfko]
gpewwmjmih-glsgspexi-jmrergmrk-542[bfqnt]
vqr-ugetgv-fag-fgxgnqrogpv-440[gvfqr]
encuukhkgf-etaqigpke-tcddkv-ceswkukvkqp-726[fijhw]
yrwxefpi-fyrrc-hiwmkr-724[rfiwy]
hwdtljsnh-bjfutsneji-hfsid-htfynsl-xfqjx-801[cdbsl]
xjmmjndqz-xcjxjgvoz-jkzmvodjin-681[jmxzd]
jqwpihizlwca-kivlg-tijwzibwzg-200[jpsbx]
fodvvlilhg-froruixo-fkrfrodwh-wudlqlqj-621[yketz]
wpuvcdng-fag-fgrctvogpv-518[klean]
xgvnndadzy-xviyt-xjvodib-mzxzdqdib-421[oypak]
kzeed-gzssd-qtlnxynhx-515[byszr]
surmhfwloh-iorzhu-pdqdjhphqw-517[bhpqy]
xcitgcpixdcpa-ytaanqtpc-rjhidbtg-htgkxrt-167[kjlxw]
yuxufmdk-sdmpq-nmewqf-emxqe-248[cteon]
iuxxuyobk-kmm-xkykgxin-306[hjxkm]
joufsobujpobm-dboez-dpbujoh-mphjtujdt-675[fwybh]
ajmrxjlcren-ouxfna-vjatncrwp-459[ajnrc]
nwzekwypera-xwogap-paydjkhkcu-992[akpwe]
aietsrmdih-gpewwmjmih-gerhc-stivexmsrw-282[eimhr]
sxdobxkdsyxkv-nio-yzobkdsyxc-874[gpnhm]
qfkkj-mldvpe-afcnsldtyr-639[dfkla]
eqnqthwn-tcddkv-ucngu-674[lekca]
lqxlxujcn-bcxajpn-667[aomrt]
etaqigpke-dcumgv-wugt-vguvkpi-180[guvei]
udglrdfwlyh-mhoobehdq-orjlvwlfv-413[mnizv]
wyvqljapsl-jhukf-jvhapun-klzpnu-851[ixkjt]
esyfwlau-jsttal-wfyafwwjafy-138[afwyj]
bdavqofuxq-ymszqfuo-otaoaxmfq-fdmuzuzs-222[zbyrv]
udskkaxawv-wyy-vwkayf-996[drsqf]
qzoggwtwsr-qczcftiz-foppwh-gvwddwbu-246[btqac]
yaxsnlcrun-yujbcrl-pajbb-nwprwnnarwp-745[cdwey]
zhdsrqlchg-gbh-pdunhwlqj-439[hdglq]
zekvierkzferc-nvrgfezqvu-treup-tfrkzex-ivjvrity-139[nthvs]
vhkkhlbox-ktwbhtvmbox-vtgwr-vhtmbgz-ftgtzxfxgm-709[tbghv]
kmjezxodgz-xmtjbzidx-agjrzm-rjmfncjk-863[hramk]
qspkfdujmf-kfmmzcfbo-vtfs-uftujoh-129[nmtql]
ipvohghykvbz-jhukf-jvhapun-jvuahputlua-591[cyqjf]
lahxpnwrl-lqxlxujcn-vjwjpnvnwc-563[wcjme]
sbejpbdujwf-qmbtujd-hsbtt-bdrvjtjujpo-493[jbtdu]
ujoon-hrpktcvtg-wjci-stepgibtci-115[tdcgy]
wlqqp-jtrmvexvi-ylek-rercpjzj-321[pyfoz]
qyujihctyx-vohhs-yhachyylcha-604[xfbmz]
eza-dpncpe-nlyoj-nzletyr-epnsyzwzrj-925[ytbnm]
nzwzcqfw-awldetn-rcldd-lnbftdtetzy-743[tomzn]
nwzekwypera-bhksan-yqopkian-oanreya-914[otvsm]
avw-zljyla-wshzapj-nyhzz-klclsvwtlua-773[lazwh]
plolwdub-judgh-iorzhu-vdohv-179[bmrjz]
lnkfaypeha-zua-zarahkliajp-368[tzngm]
slqryzjc-afmamjyrc-pcqcypaf-418[tjmsy]
frqvxphu-judgh-vfdyhqjhu-kxqw-rshudwlrqv-387[dxhnm]
uqtqbizg-ozilm-lgm-nqvivkqvo-928[rmnzf]
lqwhuqdwlrqdo-edvnhw-ilqdqflqj-335[ctsda]
zloolpfsb-mixpqfz-doxpp-obzbfsfkd-133[txiel]
otzkxtgzoutgr-igtje-iugzotm-xkykgxin-592[zywxn]
sbnqbhjoh-kfmmzcfbo-nbobhfnfou-701[fihgn]
eqttqukxg-tcfkqcevkxg-rncuvke-itcuu-vtckpkpi-960[fobts]
xgjougizobk-hatte-jkbkruvsktz-592[ktbgj]
bqvvu-bhksan-pnwejejc-836[tcjgz]
ikhcxvmbex-unggr-kxvxbobgz-787[opysz]
jsehsyafy-usfvq-mkwj-lwklafy-320[msztr]
ikhcxvmbex-ktuubm-ftgtzxfxgm-891[ojsny]
ajvyjprwp-ajkkrc-mnenuxyvnwc-381[umkpn]
pinovwgz-zbb-mznzvmxc-993[aumbt]
ajyqqgdgcb-aylbw-rcaflmjmew-678[abcgj]
houngfgxjuay-kmm-zxgototm-488[zudop]
kmjezxodgz-ezggtwzvi-ozxcijgjbt-707[jyizu]
eadalsjq-yjsvw-xdgowj-mkwj-lwklafy-762[jwald]
aczupnetwp-ojp-dstaatyr-379[ftyeo]
gvcskirmg-yrwxefpi-gerhc-gsexmrk-ywiv-xiwxmrk-958[rgixe]
yuxufmdk-sdmpq-rxaiqd-pqbmdfyqzf-196[azivn]
qyujihctyx-wuhxs-yhachyylcha-344[zdimn]
rflsjynh-idj-yjhmstqtld-515[cvfph]
lqwhuqdwlrqdo-edvnhw-rshudwlrqv-257[nlxst]
odiih-kdwwh-mnbrpw-953[wdhib]
ugdgjxmd-uzgugdslw-klgjsyw-814[cktda]
mybbycsfo-pvygob-myxdksxwoxd-744[ifscp]
bqvvu-ywjzu-odellejc-316[krcsj]
nzcczdtgp-dnlgpyrpc-sfye-cplnbftdtetzy-743[kbdyu]
pinovwgz-xcjxjgvoz-xjiovdihzio-525[mhjxn]
ckgvutofkj-igtje-xkykgxin-878[vimez]
rmn-qcapcr-zsllw-bcnjmwkclr-912[anzkg]
ipvohghykvbz-yhiipa-zlycpjlz-227[oyzjt]
nwilwcejc-nwxxep-wymqeoepekj-550[ztyes]
uzfqdzmfuazmx-otaoaxmfq-pqhqxabyqzf-274[qafzm]
hafgnoyr-qlr-chepunfvat-975[zauvm]
irdgrxzex-treup-tfrkzex-ljvi-kvjkzex-269[lozqp]
udskkaxawv-bwddqtwsf-klgjsyw-944[nmsxw]
ujoon-rpcsn-gtprfjxhxixdc-921[xcjno]
jrncbavmrq-enoovg-qrfvta-195[aqyxs]
xgsvgmotm-yigbktmkx-natz-iutzgotsktz-462[smvtd]
sxdobxkdsyxkv-mkxni-zebmrkcsxq-770[zkwax]
zbytomdsvo-oqq-crszzsxq-614[iwqoc]
wfruflnsl-kqtbjw-qfgtwfytwd-801[imsvp]
nglmtuex-wrx-ybgtgvbgz-787[etuki]
ixeumktoi-yigbktmkx-natz-uvkxgzouty-774[hylkz]
cxy-bnlanc-ljwmh-cnlqwxuxph-719[unwgt]
xcitgcpixdcpa-ytaanqtpc-hidgpvt-453[cptai]
udglrdfwlyh-fkrfrodwh-fxvwrphu-vhuylfh-985[ntvum]
dsxxw-glrcplyrgmlyj-afmamjyrc-yaosgqgrgml-912[hstuv]
nglmtuex-xzz-wxiehrfxgm-267[wjyqr]
ixccb-fdqgb-xvhu-whvwlqj-153[bchqv]
vhehkyne-vtgwr-vhtmbgz-ehzblmbvl-449[mnuht]
xgjougizobk-hgyqkz-jkyomt-774[sdeqt]
ktiaaqnqml-zijjqb-abwziom-252[fgisn]
glrcplyrgmlyj-cee-pcyaosgqgrgml-470[osmrp]
qjopwxha-lhwopey-cnwoo-nawymqeoepekj-472[rfgnc]
plolwdub-judgh-fdqgb-vklsslqj-959[ldbgj]
yknnkoera-zua-wymqeoepekj-472[rsqmj]
ujoon-ytaanqtpc-gtrtxkxcv-635[tacno]
tcfkqcevkxg-fag-ujkrrkpi-856[njiek]
uzfqdzmfuazmx-bdavqofuxq-qss-dqmocgueufuaz-170[wktxs]
ltpedcxots-rpcsn-hwxeexcv-375[draoy]
zntargvp-pnaql-nanylfvf-585[iejdf]
lqwhuqdwlrqdo-vfdyhqjhu-kxqw-orjlvwlfv-101[szrtk]
dyz-combod-oqq-cdybkqo-614[odqbc]
jyddc-fyrrc-wepiw-802[chjtm]
nglmtuex-yehpxk-phkdlahi-319[cjshn]
surmhfwloh-fdqgb-xvhu-whvwlqj-205[vzite]
wrs-vhfuhw-udeelw-dqdobvlv-179[znwyc]
myxcewob-qbkno-lkcuod-cdybkqo-328[ysbjw]
ixeumktoi-inuiurgzk-giwaoyozout-124[vnlcr]
vhehkyne-utldxm-nlxk-mxlmbgz-839[cnryt]
tagzsrsjvgmk-tskcwl-klgjsyw-216[sgkjl]
bjfutsneji-idj-ijajqturjsy-203[gkoyb]
plolwdub-judgh-gbh-wudlqlqj-335[snabj]
ydjuhdqjyedqb-sqdto-seqjydw-tufbeocudj-504[cawtu]
fkqbokxqflkxi-ciltbo-jxohbqfkd-913[mthlx]
eadalsjq-yjsvw-tskcwl-sfsdqkak-840[sakdj]
tipfxvezt-dzczkrip-xiruv-irsszk-ivtvzmzex-347[ewgst]
plolwdub-judgh-fdqgb-pdqdjhphqw-309[slvtc]
nwilwcejc-iehepwnu-cnwza-ywjzu-naoawnyd-914[wnace]
lejkrscv-irsszk-fgvirkzfej-633[krsef]
qzlozfhmf-bgnbnkzsd-qdrdzqbg-105[ltrxz]
hmsdqmzshnmzk-bkzrrhehdc-idkkxadzm-sqzhmhmf-833[vsftk]
mbggf-wyvqljapsl-ibuuf-ylhjxbpzpapvu-565[kxetg]
vehmsegxmzi-tpewxmg-kveww-erepcwmw-360[ewmgp]
mhi-lxvkxm-xzz-wxoxehifxgm-475[zvuxn]
pejji-nio-gybucryz-848[scvwe]
oqnidbshkd-rbzudmfdq-gtms-cdozqsldms-729[dsmqb]
hmsdqmzshnmzk-lzfmdshb-idkkxadzm-btrsnldq-rdquhbd-937[yncvm]
fmsledevhsyw-wgezirkiv-lyrx-jmrergmrk-932[bgytv]
houngfgxjuay-lruckx-iayzuskx-ykxboik-462[vefmw]
njmjubsz-hsbef-dboez-vtfs-uftujoh-337[bfjsu]
lhkhszqx-fqzcd-azrjds-cdozqsldms-157[amnks]
dpotvnfs-hsbef-dipdpmbuf-tbmft-103[zhrge]
sebehvkb-rqiauj-jusxdebewo-166[jscdv]
diozmivodjivg-pinovwgz-ezggtwzvi-mznzvmxc-811[ysntj]
kwtwznct-kpwkwtibm-zmamizkp-902[kwmtz]
jyddc-fyrrc-asvowlst-828[ybfgd]
tagzsrsjvgmk-vqw-ugflsafewfl-580[xmqnr]
mfklstdw-tmffq-kwjnauwk-502[fkwmt]
xcitgcpixdcpa-rpcsn-rdpixcv-rdcipxcbtci-453[ynzqc]
fmsledevhsyw-wgezirkiv-lyrx-hitevxqirx-542[eirvx]
ojk-nzxmzo-zbb-ozxcijgjbt-213[zbjox]
gpewwmjmih-hci-vigimzmrk-932[injxy]
gpsxdprixkt-qjccn-hpath-349[pchtx]
diozmivodjivg-nxvqzibzm-cpio-omvdidib-109[jivdx]
dzczkrip-xiruv-srjbvk-crsfirkfip-269[mbnda]
lgh-kwujwl-usfvq-ugslafy-vwhsjlewfl-580[ftwxr]
ymszqfuo-rxaiqd-xmnadmfadk-456[smxyt]
slqryzjc-njyqrga-epyqq-kypicrgle-158[ktcij]
apwmeclga-hcjjwzcyl-bctcjmnkclr-496[cjlam]
pejji-zvkcdsm-qbkcc-gybucryz-770[junto]
kzeed-kqtbjw-xmnuunsl-931[eknub]
fhezusjybu-sedikcuh-whqtu-hqrryj-cqhaujydw-322[kogjn]
encuukhkgf-uecxgpigt-jwpv-fgrnqaogpv-648[gpuce]
fnjyxwrinm-kjbtnc-bjunb-225[nbjcf]
gifavtkzcv-sleep-kvtyefcfxp-737[cusry]
nij-mywlyn-xsy-fiacmncwm-266[txfyk]
pxtihgbsxw-cxeeruxtg-labiibgz-501[vfryk]
oaxadrgx-vqxxknqmz-etubbuzs-612[gmzpy]
oxaflxzqfsb-yxphbq-obzbfsfkd-497[gsjqc]
lxaaxbren-ajmrxjlcren-kdwwh-uxprbcrlb-563[rxabl]
drxevkzt-jtrmvexvi-ylek-ivrthlzjzkzfe-893[evzkr]
xst-wigvix-veqtekmrk-hci-hizipstqirx-334[itxeh]
hqcfqwydw-sxesebqju-bewyijysi-556[upyan]
oxjmxdfkd-avb-abmxoqjbkq-809[cuyzr]
vkppo-isqludwuh-xkdj-husuylydw-686[sfajr]
nwilwcejc-ykjoqian-cnwza-ywjzu-ykjpwejiajp-576[jwaci]
gpewwmjmih-hci-jmrergmrk-360[jftsi]
ibghopzs-gqojsbusf-vibh-gvwddwbu-324[ratwb]
mvhkvbdib-wvnfzo-gvwjmvojmt-785[zljyf]
lahxpnwrl-mhn-bjunb-719[kexws]
iuxxuyobk-inuiurgzk-vaxingyotm-722[snwtx]
rnqnyfwd-lwfij-xhfajsljw-mzsy-wjhjnansl-671[nyphz]
uwtojhynqj-wfggny-yjhmstqtld-229[qzmtg]
dfcxsqhwzs-pibbm-qighcasf-gsfjwqs-220[iucxp]
eqttqukxg-dwppa-uvqtcig-362[setiw]
gcfcnuls-aluxy-vohhs-guleyncha-500[gchrt]
ocipgvke-dwppa-gpikpggtkpi-518[tihzs]
qfmcusbwq-dzoghwq-ufogg-cdsfohwcbg-454[ylkjh]
yhtwhnpun-msvdly-zlycpjlz-123[qjnxy]
lxuxaodu-ljwmh-lxjcrwp-anjlzdrbrcrxw-199[lnesc]
qekrixmg-jpsaiv-tyvglewmrk-646[egikm]
emixwvqhml-moo-wxmzibqwva-720[mwioq]
kpvgtpcvkqpcn-dwppa-cpcnauku-934[bmprc]
lugjuacha-vumeyn-mylpcwym-604[muyac]
lzfmdshb-ahngzyzqcntr-dff-nodqzshnmr-937[pwdof]
sbqiiyvyut-vkppo-zubboruqd-cqdqwucudj-348[osntj]
ubhatstkwhnl-wrx-ftkdxmbgz-111[fojwk]
mvhkvbdib-zbb-ncdkkdib-967[brnch]
vetllbybxw-utldxm-mktbgbgz-579[gljhz]
htsxzrjw-lwfij-gzssd-btwpxmtu-333[rhtqs]
nzcczdtgp-nlyoj-nzletyr-xlcvpetyr-587[smnct]
pybgmyargtc-zsllw-jyzmpyrmpw-366[zutsy]
kfg-jvtivk-upv-jvimztvj-997[xicjt]
kpvgtpcvkqpcn-hnqygt-ewuvqogt-ugtxkeg-388[gtkpq]
ktiaaqnqml-kpwkwtibm-mvoqvmmzqvo-564[asert]
mfklstdw-uzgugdslw-dgyaklauk-892[dgklu]
gzefmnxq-pkq-xmnadmfadk-196[madfk]
qcbgiasf-ufors-dzoghwq-ufogg-ghcfous-714[yiavh]
veqtekmrk-nippcfier-asvowlst-958[thmsn]
iehepwnu-cnwza-zua-skngodkl-134[naeku]
xfbqpojafe-fhh-tfswjdft-571[fhjta]
laffe-jek-ygrky-150[pjbaq]
lxwbdvna-pajmn-kjbtnc-fxatbqxy-459[xbmlo]
wkqxodsm-bkllsd-bomosfsxq-276[isaey]
ovbunmneqbhf-fpniratre-uhag-qrirybczrag-741[rabne]
xjmmjndqz-mvwwdo-kpmxcvndib-395[hyolt]
zbytomdsvo-mkxni-mykdsxq-bomosfsxq-276[efcvd]
jqwpihizlwca-bwx-amkzmb-jcvvg-ivitgaqa-252[dsmej]
dszphfojd-sbccju-nbobhfnfou-805[bfocd]
hwbba-lgnnadgcp-vgejpqnqia-830[ptkvu]
wfintfhynaj-hmthtqfyj-fhvznxnynts-125[nfhty]
wfummczcyx-wbiwifuny-mbcjjcha-864[cmwbf]
tfcfiwlc-avccpsvre-uvjzxe-269[cvefa]
dlhwvupglk-msvdly-dvyrzovw-331[nmeji]
fnjyxwrinm-npp-orwjwlrwp-979[tqyvp]
sgmtkzoi-hgyqkz-xkgiwaoyozout-904[ogkzi]
qjopwxha-oywrajcan-dqjp-opknwca-680[qgsnh]
oxjmxdfkd-zxkav-pxibp-185[mtfwl]
mfklstdw-wyy-lwuzfgdgyq-814[iznma]
zlilocri-bdd-pefmmfkd-341[intay]
rwcnawjcrxwju-kdwwh-cajrwrwp-849[wrcja]
xmrrq-uzgugdslw-jwuwanafy-372[uwagr]
kzgwomvqk-zijjqb-ewzsapwx-278[wzjkq]
lxuxaodu-ouxfna-ujkxajcxah-277[xaujo]
nwzekwypera-iehepwnu-cnwza-fahhuxawj-zalhkuiajp-186[nbzay]
vhkkhlbox-ktuubm-hixktmbhgl-943[byecg]
qxdwpopgsdjh-uadltg-prfjxhxixdc-141[eqzvr]
kyelcrga-zsllw-pcacgtgle-522[lcgae]
qzoggwtwsr-ksodcbwnsr-dzoghwq-ufogg-zcuwghwqg-818[dzclv]
xekdwvwnzkqo-lhwopey-cnwoo-bejwjyejc-238[ksrhc]
npmhcargjc-cee-bctcjmnkclr-782[tgrxz]
hwbba-ecpfa-eqcvkpi-ceswkukvkqp-336[kcepa]
uwtojhynqj-hmthtqfyj-wjhjnansl-437[kylvd]
sxdobxkdsyxkv-bknsykmdsfo-oqq-kmaescsdsyx-380[skdxo]
rmn-qcapcr-afmamjyrc-dglylagle-158[aclmr]
raphhxuxts-gpqqxi-advxhixrh-583[vuafx]
nij-mywlyn-xsy-jolwbumcha-760[mkhgy]
lnkfaypeha-xqjju-odellejc-680[ghsol]
ytu-xjhwjy-wfggny-zxjw-yjxynsl-931[tynvp]
myxcewob-qbkno-lkcuod-nofovyzwoxd-744[obcdk]
diozmivodjivg-mvwwdo-gvwjmvojmt-161[vpsib]
vhkkhlbox-cxeeruxtg-ehzblmbvl-787[behlx]
iruzfrtkzmv-treup-tfrkzex-jvimztvj-841[xmwvi]
kyelcrga-cee-pcacgtgle-730[yladm]
iutyaskx-mxgjk-yigbktmkx-natz-jkyomt-436[tznsm]
hvbizodx-agjrzm-ncdkkdib-421[iqnst]
xzwrmkbqtm-kivlg-lmxtwgumvb-252[tkefm]
udpsdjlqj-vfdyhqjhu-kxqw-pdqdjhphqw-439[dqhjp]
mbggf-ibuuf-mpuhujpun-695[gnpdh]
irdgrxzex-avccpsvre-ivtvzmzex-997[stcvr]
sawlkjevaz-ywjzu-opknwca-602[awjkz]
wlqqp-irsszk-tljkfdvi-jvimztv-217[mrqcl]
sehheiylu-tou-sedjqydcudj-192[teamg]
ajyqqgdgcb-pyzzgr-kylyeckclr-470[cjtul]
ziuxioqvo-moo-bziqvqvo-278[ixnjm]
vjpwncrl-kjbtnc-nwprwnnarwp-199[nwprc]
wpuvcdng-gii-hkpcpekpi-986[picgk]
dmybmsuzs-nmewqf-iadwetab-456[mabde]
rtqlgevkng-tcddkv-fgukip-830[juyzr]
votubcmf-dipdpmbuf-sfbdrvjtjujpo-727[rcslk]
vhkkhlbox-wrx-ftgtzxfxgm-163[txmwq]
zloolpfsb-mixpqfz-doxpp-qbzeklildv-289[lpozb]
rkpqxyib-zxkav-zlxqfkd-xkxivpfp-393[bpquj]
pbafhzre-tenqr-qlr-freivprf-455[byihv]
kzeed-hfsid-htfynsl-jslnsjjwnsl-905[egluv]
nlyoj-nzletyr-lylwjdtd-899[ymzvu]
laffe-pkrrehkgt-sgxqkzotm-800[gsdnt]
ynukcajey-xwogap-zaoecj-212[ewjah]
rgndvtcxr-bxaxipgn-vgpst-hrpktcvtg-wjci-sthxvc-661[zwgis]
ujqgywfau-vqw-vwnwdghewfl-580[istzh]
jyddc-tvsnigxmpi-fewoix-hiwmkr-178[idmwx]
fab-eqodqf-omzpk-etubbuzs-430[befoq]
ibghopzs-suu-fsoqeiwgwhwcb-792[qzyrk]
sedikcuh-whqtu-uww-vydqdsydw-166[dwuhq]
cjpibabsepvt-qmbtujd-hsbtt-tupsbhf-467[wsrek]
aietsrmdih-ikk-gsrxemrqirx-594[irekm]
bwx-amkzmb-jcvvg-bziqvqvo-694[vbmqz]
ixccb-exqqb-xvhu-whvwlqj-647[qxbch]
nwlddtqtpo-nlyoj-dezclrp-639[dlnop]
ahngzyzqcntr-azrjds-otqbgzrhmf-833[oquzx]
xcitgcpixdcpa-gpqqxi-ejgrwphxcv-193[cpxgi]
buzahisl-zjhclunly-obua-svnpzapjz-643[zalub]
excdklvo-mkxni-mykdsxq-nozkbdwoxd-458[fxjwy]
jvyyvzpcl-jyfvnlupj-msvdly-klclsvwtlua-253[injkl]
emixwvqhml-akidmvomz-pcvb-zmkmqdqvo-434[mvqdi]
dszphfojd-qmbtujd-hsbtt-sfbdrvjtjujpo-831[ykcho]
kwzzwaqdm-zijjqb-camz-bmabqvo-902[zabmq]
nvrgfezqvu-irsszk-jkfirxv-789[neysm]
yflexwxoalrp-zelzlixqb-jxohbqfkd-679[lxbef]
wfummczcyx-yaa-nluchcha-344[rmpgs]
tcfkqcevkxg-gii-ocpcigogpv-674[stfma]
nwzekwypera-ywjzu-ajcejaanejc-316[xoprq]
qzlozfhmf-idkkxadzm-btrsnldq-rdquhbd-105[pawus]
uqtqbizg-ozilm-kivlg-uizsmbqvo-356[xhvzm]
apuut-kgvnodx-bmvnn-mzvxlpdndodji-473[dnvmo]
wfummczcyx-dyffsvyuh-guhuaygyhn-500[yufhc]
cybyjqho-whqtu-shoewudys-vbemuh-tuiywd-842[huywb]
sedikcuh-whqtu-tou-jhqydydw-920[dhuqt]
zekvierkzferc-wcfnvi-ljvi-kvjkzex-815[ibzsg]
froruixo-udeelw-orjlvwlfv-777[zyusv]
oxmeeuruqp-ngzzk-qzsuzqqduzs-222[mcqzj]
gpewwmjmih-ikk-ywiv-xiwxmrk-568[uszki]
aietsrmdih-jpsaiv-wepiw-334[iaeps]
joufsobujpobm-cbtlfu-dvtupnfs-tfswjdf-441[fubjo]
xst-wigvix-hci-viwievgl-256[jnlst]
oknkvcta-itcfg-wpuvcdng-dwppa-qrgtcvkqpu-414[dcxst]
rnqnyfwd-lwfij-gfxpjy-htsyfnsrjsy-957[whlxr]
sbnqbhjoh-fhh-tbmft-181[jndic]
slqryzjc-afmamjyrc-qcptgacq-236[soytq]
rgllk-qss-ymdwqfuzs-768[ynsjx]
nwilwcejc-ywjzu-ykwpejc-pnwejejc-966[fthzx]
gokzyxsjon-lexxi-gybucryz-146[qomrj]
cybyjqho-whqtu-zubboruqd-cqhaujydw-192[naxrp]
esyfwlau-bwddqtwsf-vwkayf-294[acbmr]
shmml-rtt-znantrzrag-455[vseqh]
qlm-pbzobq-yrkkv-obpbxoze-601[lfewg]
fbebmtkr-zktwx-utldxm-ybgtgvbgz-995[stygr]
ikhcxvmbex-ietlmbv-zktll-vnlmhfxk-lxkobvx-943[lxkvb]
rgndvtcxr-hrpktcvtg-wjci-prfjxhxixdc-193[crxtd]
ucynmlgxcb-pyzzgr-kypicrgle-158[qtuyz]
wpuvcdng-lgnnadgcp-vtckpkpi-466[sftpn]
tfiifjzmv-treup-kirzezex-633[gjtxn]
cvabijtm-jiasmb-lmxizbumvb-356[axbtn]
xgvnndadzy-kgvnodx-bmvnn-yzkgjthzio-395[byaut]
rdchjbtg-vgpst-tvv-pcpanhxh-765[hptvc]
hqcfqwydw-isqludwuh-xkdj-cqhaujydw-322[mzgty]
hvbizodx-wvnfzo-gjbdnodxn-577[kljip]
udpsdjlqj-fruurvlyh-fdqgb-frdwlqj-vdohv-283[ktzrm]
xjmmjndqz-zbb-jkzmvodjin-707[jmzbd]
glrcplyrgmlyj-zsllw-nspafyqgle-184[ptidc]
zvyvgnel-tenqr-cebwrpgvyr-fpniratre-uhag-hfre-grfgvat-377[zesyf]
ubhatstkwhnl-vahvhetmx-wxitkmfxgm-605[qzvme]
iqmbazulqp-dmnnuf-ruzmzouzs-898[shtag]
surmhfwloh-gbh-ghsorbphqw-179[sickj]
htqtwkzq-uqfxynh-lwfxx-xjwanhjx-593[rwtbd]
kgjgrypw-epybc-njyqrga-epyqq-rpyglgle-600[jkcuz]
ktiaaqnqml-zijjqb-zmikycqaqbqwv-642[qaibj]
udglrdfwlyh-fdqgb-frdwlqj-sxufkdvlqj-803[cmkaq]
oxmeeuruqp-pkq-pqbmdfyqzf-196[qpefm]
dzczkrip-xiruv-sleep-ivjvrity-893[xjomn]
dmpuamofuhq-ymszqfuo-otaoaxmfq-fdmuzuzs-482[mufoa]
nbhofujd-ezf-tbmft-389[cjgtr]
uzfqdzmfuazmx-otaoaxmfq-iadwetab-820[afmzd]
froruixo-fdqgb-oderudwrub-205[ziyjo]
ugjjgkanw-kusnwfywj-zmfl-dstgjslgjq-840[nhxgt]
zhdsrqlchg-iorzhu-sxufkdvlqj-101[qtzca]
dsxxw-aylbw-amyrgle-qcptgacq-912[qdwmn]
veqtekmrk-hci-xiglrspskc-282[tfuln]
qcffcgwjs-forwcoqhwjs-rms-hfowbwbu-480[hgnsl]
jvsvymbs-jovjvshal-zhslz-539[ocmvj]
enqvbnpgvir-rtt-fuvccvat-195[nmrlc]
oxmeeuruqp-bxmefuo-sdmee-geqd-fqefuzs-170[uenfm]
wlsiayhcw-wfummczcyx-xsy-xyjfisgyhn-214[zpqtr]
jxdkbqfz-avb-obxznrfpfqflk-133[fbkqx]
zvyvgnel-tenqr-pnaql-bcrengvbaf-195[eghnt]
diozmivodjivg-ytz-vxlpdndodji-551[diovj]
jyddc-jpsaiv-ywiv-xiwxmrk-308[idjvw]
pbafhzre-tenqr-enoovg-hfre-grfgvat-403[erfga]
pbeebfvir-enoovg-ernpdhvfvgvba-793[vebfg]
wfummczcyx-zfiqyl-uhufsmcm-292[tvcdw]
kwvacumz-ozilm-zijjqb-ivitgaqa-746[iazjm]
udpsdjlqj-ixccb-sodvwlf-judvv-ghyhorsphqw-465[kgyhw]
tcrjjzwzvu-wcfnvi-rthlzjzkzfe-451[zjcfr]
qxdwpopgsdjh-rpcsn-htgkxrth-947[dkwlb]
yaxsnlcrun-kdwwh-bnaerlnb-953[zyncx]
zhdsrqlchg-vfdyhqjhu-kxqw-xvhu-whvwlqj-699[mriyj]
bkzrrhehdc-bgnbnkzsd-lzqjdshmf-417[izpxt]
nwzekwypera-fahhuxawj-wjwhuoeo-862[wsmnu]
zgmfyxypbmsq-bwc-rcaflmjmew-964[nmpwb]
guahyncw-wuhxs-wihnuchgyhn-188[vmayn]
kwtwznct-jcvvg-uizsmbqvo-980[ryzva]
udskkaxawv-lgh-kwujwl-uzgugdslw-vwhdgqewfl-138[trijm]
shoewudys-rkddo-huiuqhsx-868[tozlb]
bnknqetk-atmmx-rzkdr-183[zbyuw]
hwdtljsnh-idj-xytwflj-983[sztqp]
tipfxvezt-jtrmvexvi-ylek-cfxzjkztj-165[gwmzp]
clotzlnetgp-nsznzwlep-opdtry-327[khryz]
mhi-lxvkxm-bgmxkgtmbhgte-wrx-ltexl-449[cfrql]
hqtyeqsjylu-sxesebqju-cqdqwucudj-686[ngaly]
zlkprjbo-doxab-avb-ildfpqfzp-211[ponbm]
bqvvu-zua-ykjpwejiajp-420[mdlgx]
tfcfiwlc-treup-tfrkzex-drerxvdvek-841[fmlyq]
rmn-qcapcr-qaytclecp-fslr-bctcjmnkclr-652[zksta]
oazegyqd-sdmpq-nmewqf-xasuefuoe-248[qypjm]
kwtwznct-lgm-tijwzibwzg-278[ytdmc]
wlqqp-gcrjkzt-xirjj-rthlzjzkzfe-555[xzkhl]
gpewwmjmih-gerhc-erepcwmw-646[stnzy]
dmybmsuzs-qss-eqdhuoqe-144[sqdem]
yflexwxoalrp-zelzlixqb-qoxfkfkd-705[gtnxw]
votubcmf-cvooz-sftfbsdi-701[qhtkp]
iuruxlar-xghhoz-gtgreyoy-306[fnctb]
slqryzjc-bwc-qyjcq-574[pozts]
wdjcvuvmyjpn-nxvqzibzm-cpio-gjbdnodxn-525[pcmhn]
excdklvo-mkxni-mykdsxq-vklybkdybi-744[vmsba]
pybgmyargtc-hcjjwzcyl-sqcp-rcqrgle-912[nhxvg]
wifilzof-wuhxs-xypyfijgyhn-760[nuovc]
ktiaaqnqml-jcvvg-lmaqov-512[nwxms]
kfg-jvtivk-tyftfcrkv-crsfirkfip-945[fkirt]
tfiifjzmv-avccpsvre-ivjvrity-581[fnemb]
zotts-wuhxs-wiuncha-xyjfisgyhn-786[tzjse]
ynssr-ktuubm-phkdlahi-579[bemfv]
xlrypetn-mldvpe-dlwpd-145[rnfmz]
mfklstdw-wyy-wfyafwwjafy-840[lksij]
dsxxw-aylbw-amlryglkclr-938[larwx]
wyvqljapsl-msvdly-mpuhujpun-435[gzlnx]
bxaxipgn-vgpst-hrpktcvtg-wjci-steadnbtci-323[tcgip]
oxjmxdfkd-mixpqfz-doxpp-abpfdk-471[dpxfk]
tipfxvezt-szfyrqriuflj-avccpsvre-ivrthlzjzkzfe-321[zfrve]
qyujihctyx-mwupyhayl-bohn-uwkocmcncih-708[chyui]
zilqwikbqdm-kivlg-apqxxqvo-434[obtmp]
willimcpy-luvvcn-qilembij-578[ilcmv]
rflsjynh-kqtbjw-ijxnls-437[zskut]
jsvagsulanw-hdsklau-yjskk-klgjsyw-814[uzmvg]
oknkvcta-itcfg-tcddkv-yqtmujqr-908[zdync]
zvyvgnel-tenqr-pnaql-pbngvat-qrcyblzrag-741[gvtck]
xjgjmapg-zbb-adivixdib-395[ovzan]
gvcskirmg-tpewxmg-kveww-wlmttmrk-464[szvty]
gpewwmjmih-gerhc-gsexmrk-wlmttmrk-620[fqrba]
jchipqat-gpqqxi-jhtg-ithixcv-661[ihqtc]
bjfutsneji-ojqqdgjfs-ijufwyrjsy-619[jfsiq]
gvcskirmg-fyrrc-hitpscqirx-308[kwgpv]
mvhkvbdib-wpiit-rjmfncjk-525[stlem]
bnqqnrhud-qzaahs-cdrhfm-339[pbsax]
qfmcusbwq-qvcqczohs-rsgwub-116[qcsbu]
qyujihctyx-yaa-guhuaygyhn-734[yahug]
jqwpihizlwca-kivlg-kwibqvo-uizsmbqvo-616[qozkj]
nglmtuex-vtgwr-ybgtgvbgz-865[gtbve]
encuukhkgf-ecpfa-eqcvkpi-yqtmujqr-414[cekqu]
jchipqat-ytaanqtpc-tcvxcttgxcv-375[jnfcy]
mvydjvxodqz-xviyt-xjvodib-vxlpdndodji-915[spxnt]
wfintfhynaj-hmthtqfyj-xmnuunsl-151[nfhtj]
pynffvsvrq-sybjre-ratvarrevat-611[rvaef]
hplazytkpo-dnlgpyrpc-sfye-nfdezxpc-dpcgtnp-327[uzifr]
cybyjqho-whqtu-vbemuh-qdqboiyi-374[zlsdw]
yhkpvhjapcl-wshzapj-nyhzz-svnpzapjz-825[bpcty]
zhdsrqlchg-mhoobehdq-fxvwrphu-vhuylfh-179[hdflo]
ynukcajey-ywjzu-ykwpejc-qoan-paopejc-238[bpemf]
vehmsegxmzi-gerhc-gsexmrk-pefsvexsvc-256[umtvy]
lugjuacha-jfumncw-alumm-uwkocmcncih-240[bzlaw]
nvrgfezqvu-jtrmvexvi-ylek-jkfirxv-165[ywnzb]
fhezusjybu-uww-fkhsxqiydw-920[hgusy]
bgmxkgtmbhgte-vahvhetmx-hixktmbhgl-189[ebaun]
apwmeclga-afmamjyrc-pcqcypaf-496[uyatz]
pdjqhwlf-vfdyhqjhu-kxqw-whfkqrorjb-829[xogzy]
crwwv-rkpqxyib-zxkav-rpbo-qbpqfkd-289[bkpqr]
tfcfiwlc-irsszk-kvtyefcfxp-893[fciks]
lejkrscv-wcfnvi-ivjvrity-191[uyalq]
ktfitzbgz-unggr-inkvatlbgz-111[lmwni]
fodvvlilhg-fdqgb-frdwlqj-frqwdlqphqw-205[kxstn]
odkasqzuo-bxmefuo-sdmee-fqotzaxask-638[oaesd]
pdjqhwlf-fdqgb-frdwlqj-rshudwlrqv-569[xcdnu]
ckgvutofkj-vrgyzoi-mxgyy-jkvgxzsktz-592[murta]
eza-dpncpe-mtzslklcozfd-clmmte-dezclrp-353[celzd]
vehmsegxmzi-nippcfier-wxsveki-516[vqrpz]
rflsjynh-jll-ijufwyrjsy-853[taqil]
jvyyvzpcl-msvdly-hjxbpzpapvu-513[nyxtz]
iehepwnu-cnwza-fahhuxawj-oanreyao-264[ulyvn]
gcfcnuls-aluxy-wbiwifuny-omyl-nymncha-526[snoxf]
oaxadrgx-dmpuamofuhq-omzpk-etubbuzs-638[uamob]
oxmeeuruqp-nmewqf-dqeqmdot-352[kjtia]
bqxnfdmhb-dff-btrsnldq-rdquhbd-521[iyjvl]
joufsobujpobm-fhh-pqfsbujpot-701[obfjp]
hafgnoyr-pnaql-pbngvat-fnyrf-845[vtsod]
mhi-lxvkxm-wrx-kxvxbobgz-657[npmfu]
jrncbavmrq-pnaql-chepunfvat-299[tnzyx]
xgsvgmotm-lruckx-gtgreyoy-566[cuxte]
vcibutulxiom-wbiwifuny-xyjfisgyhn-994[uayrz]
wyvqljapsl-zjhclunly-obua-vwlyhapvuz-123[stmdx]
tvsnigxmpi-jpsaiv-stivexmsrw-568[lhxnw]
bnqqnrhud-azrjds-cdrhfm-703[pozyr]
mfklstdw-tmffq-ugflsafewfl-164[flmst]
tcorcikpi-hnqygt-fgukip-778[hzsum]
amppmqgtc-hcjjwzcyl-qfgnngle-262[yfnvu]
ynssr-vtgwr-ehzblmbvl-631[zxbiy]
slqryzjc-aylbw-amyrgle-dglylagle-938[lyage]
vrurcjah-pajmn-snuuhknjw-jwjuhbrb-745[dphsv]
oaddaeuhq-eomhqzsqd-tgzf-etubbuzs-222[inrke]
yrwxefpi-veffmx-qerekiqirx-542[efirx]
nchhg-rmttgjmiv-bziqvqvo-304[vghim]
vkppo-vbemuh-efuhqjyedi-244[fetmj]
tyepcyletzylw-nlyoj-nzletyr-qtylyntyr-379[ngrvh]
otzkxtgzoutgr-hatte-giwaoyozout-228[tjxcr]
rgllk-eomhqzsqd-tgzf-pqbxakyqzf-534[lzpxc]
zgmfyxypbmsq-zyqicr-jyzmpyrmpw-704[ewjdl]
vehmsegxmzi-gerhc-gsexmrk-qevoixmrk-204[tyisn]
lugjuacha-zfiqyl-xyjfisgyhn-890[fcsxk]
qjopwxha-ywjzu-ykwpejc-zaoecj-810[rvmet]
bnknqetk-sno-rdbqds-idkkxadzm-knfhrshbr-521[kdnbr]
ksodcbwnsr-tzcksf-cdsfohwcbg-818[mnker]
vdzonmhydc-rbzudmfdq-gtms-lzqjdshmf-183[abjvy]
wfummczcyx-luvvcn-yhachyylcha-630[cyhal]
zvyvgnel-tenqr-rtt-freivprf-351[zymsx]
fhezusjybu-sqdto-jusxdebewo-972[amzjn]
amjmpdsj-zyqicr-cleglccpgle-106[vdopa]
hjgbwuladw-wyy-kwjnauwk-502[mrbin]
willimcpy-vohhs-mbcjjcha-734[vsbfq]
xjgjmapg-xcjxjgvoz-omvdidib-317[wynjr]
htqtwkzq-idj-xytwflj-385[tjqwd]
ynssr-xzz-xgzbgxxkbgz-579[nsivc]
xgvnndadzy-agjrzm-rjmfncjk-473[toazb]
pbeebfvir-sybjre-fuvccvat-533[stklm]
jvuzbtly-nyhkl-ibuuf-aljouvsvnf-851[lmnuf]
ziuxioqvo-ntwemz-uiviomumvb-902[arxeu]
vrurcjah-pajmn-ajkkrc-mnbrpw-589[qmtsy]
dwbcjkun-bljenwpna-qdwc-vjwjpnvnwc-537[nwjcb]
oxjmxdfkd-mixpqfz-doxpp-jxkxdbjbkq-913[xdjkp]
mvkccspson-mkxni-mykdsxq-vyqscdsmc-432[stfwy]
rflsjynh-htwwtxnaj-jll-qtlnxynhx-879[jsdzt]
pelbtravp-onfxrg-chepunfvat-663[hazbg]
zbytomdsvo-zvkcdsm-qbkcc-vyqscdsmc-692[csdmv]
qspkfdujmf-fhh-vtfs-uftujoh-415[nmzht]
aczupnetwp-nlyoj-qtylyntyr-405[tjvzp]
gsvvswmzi-jyddc-glsgspexi-wxsveki-646[puyzv]
rgndvtcxr-eaphixr-vgphh-sthxvc-973[ijhlr]
dyz-combod-mrymyvkdo-domrxyvyqi-250[pjlom]
xst-wigvix-fewoix-jmrergmrk-646[irxeg]
xgsvgmotm-kmm-iutzgotsktz-436[mtgko]
nzwzcqfw-awldetn-rcldd-nzyeltyxpye-769[delnw]
crwwv-oxyyfq-rpbo-qbpqfkd-393[qbfop]
wlsiayhcw-jfumncw-alumm-lywycpcha-344[wfzqm]
qfkkj-fydelmwp-nlyoj-nzletyr-cpnptgtyr-951[oxpuz]
qcffcgwjs-qobrm-rsdofhasbh-376[zwyat]
jshzzpmplk-pualyuhapvuhs-yhiipa-zopwwpun-669[simvn]
nglmtuex-wrx-kxvxbobgz-735[phwgz]
oaxadrgx-nmewqf-abqdmfuaze-404[tysvj]
vkppo-zubboruqd-qdqboiyi-998[dhjui]
ajvyjprwp-snuuhknjw-dbna-cnbcrwp-667[ziuhp]
pybgmyargtc-zsllw-bcnjmwkclr-314[mnhck]
pkl-oaynap-xwogap-opknwca-992[nhagy]
hafgnoyr-onfxrg-znexrgvat-741[cywdg]
vkppo-sqdto-ijehqwu-816[opqde]
jvsvymbs-jhukf-zavyhnl-409[vhjsy]
hqtyeqsjylu-cqwdujys-tou-husuylydw-686[kcdsj]
awzwhofm-ufors-pibbm-rsdofhasbh-298[bfhos]
xjinphzm-bmvyz-nxvqzibzm-cpio-mznzvmxc-161[oslup]
tfejldvi-xiruv-wcfnvi-fgvirkzfej-529[sgtbw]
qlm-pbzobq-ciltbo-pxibp-185[zjmpt]
guahyncw-xsy-xymcah-630[fckyn]
lahxpnwrl-ljwmh-jlzdrbrcrxw-407[lrwhj]
dmybmsuzs-omzpk-ogefayqd-eqdhuoq-222[mctji]
forwcoqhwjs-pibbm-twbobqwbu-480[bwoqc]
mhi-lxvkxm-lvtoxgzxk-angm-xgzbgxxkbgz-839[lczyd]
iqmbazulqp-rxaiqd-mocgueufuaz-378[ygdtb]
plolwdub-judgh-udeelw-uhfhlylqj-309[ludhe]
raphhxuxts-hrpktcvtg-wjci-htgkxrth-739[htrxc]
pybgmyargtc-hcjjwzcyl-rcaflmjmew-548[ajlkr]
lnkfaypeha-ywjzu-ykwpejc-ykjpwejiajp-368[jpyae]
vehmsegxmzi-gerhc-gsexmrk-hizipstqirx-100[jzboa]
pdjqhwlf-frqvxphu-judgh-iorzhu-frqwdlqphqw-725[yijon]
kzgwomvqk-kivlg-kwibqvo-ivitgaqa-720[zjyxm]
xqvwdeoh-surmhfwloh-fkrfrodwh-dqdobvlv-855[xwtjs]
xgsvgmotm-igtje-iugzotm-sgxqkzotm-228[ygzda]
lxwbdvna-pajmn-snuuhknjw-bcxajpn-953[yxark]
oaddaeuhq-omzpk-oamfuzs-mocgueufuaz-326[odcqb]
jyddc-ikk-hitevxqirx-750[btmqn]
oaddaeuhq-rxaiqd-fqotzaxask-586[slgad]
kwzzwaqdm-moo-nqvivkqvo-798[epnyr]
fydelmwp-nsznzwlep-opalcexpye-847[ltbwy]
dszphfojd-cbtlfu-tupsbhf-415[fbdhp]
hqcfqwydw-rkddo-skijecuh-iuhlysu-114[ynqsd]
ixeumktoi-inuiurgzk-uvkxgzouty-410[acqzt]
wlqqp-avccpsvre-wzeretzex-347[bgzvw]
vdzonmhydc-bzmcx-bnmszhmldms-157[rspcq]
fodvvlilhg-mhoobehdq-whfkqrorjb-699[defpm]
gntmfefwitzx-ojqqdgjfs-qfgtwfytwd-827[sypzf]
nzydfxpc-rclop-mfyyj-wlmzclezcj-769[clyzf]
zuv-ykixkz-pkrrehkgt-rumoyzoiy-852[yhkru]
aietsrmdih-glsgspexi-vigimzmrk-802[sgnmt]
rgndvtcxr-hrpktcvtg-wjci-igpxcxcv-245[xszoq]
nwzekwypera-xqjju-skngodkl-758[kejnw]
hcd-gsqfsh-rms-rsdzcmasbh-974[vtsqm]
qspkfdujmf-dboez-mphjtujdt-935[diolt]
aczupnetwp-nlyoj-afcnsldtyr-665[zemik]
aoubshwq-qobrm-qcohwbu-rsdofhasbh-376[wyzxl]
xtwtelcj-rclop-ncjzrpytn-ojp-qtylyntyr-353[lyris]
zvyvgnel-tenqr-pelbtravp-rtt-fnyrf-819[fjysz]
ejpanjwpekjwh-bhksan-paydjkhkcu-420[jkahp]
ygcrqpkbgf-hnqygt-hkpcpekpi-882[pgkch]
raphhxuxts-gpqqxi-pcpanhxh-817[qinta]
gsvvswmzi-wgezirkiv-lyrx-stivexmsrw-646[isvrw]
htqtwkzq-kqtbjw-fsfqdxnx-307[ytxis]
oqnidbshkd-eknvdq-cdozqsldms-833[dqskn]
qxdwpopgsdjh-hrpktcvtg-wjci-hidgpvt-219[tdlsw]
fnjyxwrinm-snuuhknjw-jwjuhbrb-459[vjwxy]
egdytrixat-eaphixr-vgphh-stepgibtci-609[vrzoq]
veqtekmrk-fewoix-pskmwxmgw-906[ekmwx]
xmrrq-eadalsjq-yjsvw-usfvq-vwnwdghewfl-502[wqsva]
dpotvnfs-hsbef-gvaaz-sbccju-tfswjdft-649[lbipa]
zvyvgnel-tenqr-enoovg-ybtvfgvpf-637[kchij]
pynffvsvrq-cynfgvp-tenff-ybtvfgvpf-247[dcwfm]
xjinphzm-bmvyz-xviyt-xjvodib-zibdizzmdib-187[gbnqs]
ugfkmewj-yjsvw-hdsklau-yjskk-dgyaklauk-502[kajsu]
iruzfrtkzmv-sleep-tljkfdvi-jvimztv-269[vitze]
sawlkjevaz-oywrajcan-dqjp-hkceopeyo-550[gvitx]
wfummczcyx-wifilzof-vumeyn-ijyluncihm-604[lnmdw]
lahxpnwrl-kdwwh-xynajcrxwb-927[xykwv]
xekdwvwnzkqo-fahhuxawj-oanreyao-836[xnzsy]
mbiyqoxsm-bkllsd-bokmaescsdsyx-718[thcfp]
uiovmbqk-zijjqb-zmamizkp-746[xpstr]
nglmtuex-vkrhzxgbv-xzz-ehzblmbvl-293[zblvx]
xgvnndadzy-kgvnodx-bmvnn-xpnojhzm-nzmqdxz-577[qgfhc]
zntargvp-ohaal-jbexfubc-975[vflzg]
vqr-ugetgv-tcddkv-gpikpggtkpi-830[zlyxv]
qfkkj-nsznzwlep-nzyeltyxpye-847[satrm]
dwbcjkun-yujbcrl-pajbb-fxatbqxy-225[glisn]
tyftfcrkv-tljkfdvi-jvimztv-113[tvfij]
hjgbwuladw-ugjjgkanw-tskcwl-suimakalagf-450[cneum]
vkrhzxgbv-wrx-etuhktmhkr-397[fnspx]
elrkdcdugrxv-zhdsrqlchg-edvnhw-vklsslqj-933[dlhrs]
etaqigpke-lgnnadgcp-uvqtcig-752[ojdgw]
nsyjwsfyntsfq-hmthtqfyj-ywfnsnsl-385[sfnyt]
bnmrtldq-fqzcd-bgnbnkzsd-sqzhmhmf-833[jipnm]
fkqbokxqflkxi-bdd-xkxivpfp-471[rjlxc]
lnkfaypeha-bhksan-hwxknwpknu-316[nsoam]
pelbtravp-ohaal-erprvivat-221[kdvcs]
dfcxsqhwzs-qobrm-qcohwbu-ghcfous-350[choqs]
ejpanjwpekjwh-xwogap-ajcejaanejc-706[dtcks]
gzefmnxq-rxaiqd-pqhqxabyqzf-638[yntmj]
awzwhofm-ufors-qobrm-qcohwbu-hsqvbczcum-454[tfiay]
yknnkoera-yhwooebeaz-lhwopey-cnwoo-klanwpekjo-420[fqbwp]
bkzrrhehdc-bzmcx-bnzshmf-cdoknxldms-183[bcdhm]
htqtwkzq-jll-jslnsjjwnsl-905[oicay]
npmhcargjc-bwc-sqcp-rcqrgle-834[thaxs]
njmjubsz-hsbef-dboez-dvtupnfs-tfswjdf-909[qkeyv]
qyujihctyx-wuhxs-wiuncha-nywbhifias-994[hiuwy]
vetllbybxw-unggr-phkdlahi-475[rqpto]
etyyx-dff-qdzbpthrhshnm-313[zpisy]
ijmockjgz-jwezxo-nojmvbz-993[jozmb]
xgvnndadzy-ytz-mznzvmxc-343[uhztm]
pxtihgbsxw-yehpxk-wxitkmfxgm-189[clynd]
tfiifjzmv-treup-tfekrzedvek-711[gptfw]
sedikcuh-whqtu-rqiauj-iuhlysui-660[znymv]
ktwbhtvmbox-ynssr-cxeeruxtg-tvjnblbmbhg-813[kdico]
xst-wigvix-wgezirkiv-lyrx-tyvglewmrk-542[igrvw]
gzefmnxq-fab-eqodqf-pkq-pqhqxabyqzf-222[examq]
ncjzrpytn-nlyoj-nzletyr-ecltytyr-925[jybxu]
mvkccspson-lexxi-crszzsxq-588[jirxq]
zbytomdsvo-bkllsd-ecob-docdsxq-640[getaf]
willimcpy-luvvcn-mbcjjcha-552[snrxz]
htsxzrjw-lwfij-gntmfefwitzx-hfsid-htfynsl-xmnuunsl-307[fnsth]
iuruxlar-jek-jkvruesktz-306[btnac]
cqwdujys-rqiauj-efuhqjyedi-166[rgncm]
dpssptjwf-dipdpmbuf-sfbdrvjtjujpo-623[yeftd]
pbybeshy-onfxrg-qrfvta-195[wmjer]
laffe-pkrrehkgt-uvkxgzouty-644[kefgr]
rnqnyfwd-lwfij-jll-fhvznxnynts-723[qmybf]
dyz-combod-lkcuod-wkbuodsxq-354[suqhl]
gpewwmjmih-veffmx-ywiv-xiwxmrk-464[zyowm]
xfbqpojafe-cvooz-bdrvjtjujpo-363[qgvxl]
wyvqljapsl-kfl-huhsfzpz-435[yomjs]
hqcfqwydw-rqiauj-huiuqhsx-478[kxtls]
ajyqqgdgcb-zsllw-qrmpyec-392[nztsu]
luxciuwncpy-zfiqyl-xypyfijgyhn-838[bzpxo]
kdijqrbu-fbqijys-whqii-udwyduuhydw-556[ihdkv]
mfklstdw-bwddqtwsf-ghwjslagfk-450[esbmd]
lugjuacha-dyffsvyuh-nywbhifias-578[snqwt]
ibghopzs-qobrm-qcohwbu-fsoqeiwgwhwcb-298[ndtlm]
nwilwcejc-oywrajcan-dqjp-hkceopeyo-394[cejow]
vqr-ugetgv-tcddkv-ucngu-258[xetvd]
rtt-genvavat-975[taveg]
willimcpy-dyffsvyuh-xyjfisgyhn-656[mrjtz]
dmbttjgjfe-njmjubsz-hsbef-gmpxfs-dpoubjonfou-779[yakzv]
muqfedyput-rqiauj-kiuh-juijydw-660[rkonp]
pkl-oaynap-fahhuxawj-odellejc-342[xmeyn]
qlm-pbzobq-yrkkv-pbosfzbp-315[bpkoq]
wihmogyl-aluxy-vumeyn-ijyluncihm-292[yilmu]
mvydjvxodqz-wvnfzo-nvgzn-291[vnzdo]
ikhcxvmbex-ktuubm-hixktmbhgl-215[bhkmx]
willimcpy-vohhs-xypyfijgyhn-136[pmnwz]
ynukcajey-xwogap-skngodkl-394[enjif]
ynukcajey-xqjju-zalwnpiajp-394[kzjlp]
yhtwhnpun-jovjvshal-hjxbpzpapvu-539[ycldk]
nglmtuex-ktuubm-ybgtgvbgz-397[qvtsp]
cvabijtm-rmttgjmiv-lmxtwgumvb-174[zpytf]
fnjyxwrinm-ajkkrc-mnyuxhvnwc-459[molxs]
mybbycsfo-mkxni-yzobkdsyxc-614[ybckm]
ugdgjxmd-kusnwfywj-zmfl-ghwjslagfk-840[gfjwd]
wbhsfbohwcboz-dzoghwq-ufogg-cdsfohwcbg-766[obghw]
qekrixmg-ikk-qerekiqirx-334[aeldt]
ktwbhtvmbox-mhi-lxvkxm-utldxm-lmhktzx-657[yqjuz]
ugdgjxmd-wyy-klgjsyw-216[gydjw]
ucynmlgxcb-cee-pcacgtgle-704[tonxb]
nsyjwsfyntsfq-hmthtqfyj-ijxnls-931[rdmog]
enzcntvat-ohaal-ybtvfgvpf-455[cvmts]
aczupnetwp-nsznzwlep-dlwpd-483[ticoy]
wbhsfbohwcboz-pibbm-cdsfohwcbg-350[oebpc]
shmml-cynfgvp-tenff-znexrgvat-715[fyuze]
sedikcuh-whqtu-uww-huiuqhsx-582[ytsoz]
ynukcajey-pkl-oaynap-xwogap-nayaerejc-550[mnfkp]
ipvohghykvbz-zjhclunly-obua-huhsfzpz-851[mecug]
tbxmlkfwba-oxyyfq-ixyloxqlov-783[woqrj]
zilqwikbqdm-kpwkwtibm-abwziom-954[yzmul]
kwvacumz-ozilm-zijjqb-apqxxqvo-590[dojfm]
bqxnfdmhb-bgnbnkzsd-sdbgmnknfx-547[ajynr]
sbnqbhjoh-dmbttjgjfe-dipdpmbuf-gjobodjoh-103[bjdoh]
jsvagsulanw-usfvq-vwhsjlewfl-268[kiwfy]
mbggf-buzahisl-lnn-wbyjohzpun-539[bnghl]
htwwtxnaj-jll-ijajqturjsy-411[zhgpj]
amppmqgtc-aylbw-amyrgle-pcacgtgle-496[agclm]
jchipqat-uadltg-detgpixdch-895[wogif]
ktfitzbgz-unggr-tgterlbl-605[wjmxs]
yhwooebeaz-ywjzu-ykwpejc-bejwjyejc-498[sgtmz]
muqfedyput-fbqijys-whqii-cqhaujydw-634[pcekt]
udskkaxawv-mfklstdw-hdsklau-yjskk-dstgjslgjq-840[xeguw]
hqfxxnknji-kqtbjw-qfgtwfytwd-983[fqtwj]
veqtekmrk-fyrrc-eguymwmxmsr-490[stfrm]
awzwhofm-ufors-ksodcbwnsr-tzcksf-gozsg-298[sofwz]
tinnm-gqojsbusf-vibh-gsfjwqsg-714[dczyw]
nuatmlmdpage-ngzzk-pqbxakyqzf-456[azgkm]
htsxzrjw-lwfij-kqtbjw-jslnsjjwnsl-255[ldqwi]
zotts-zfiqyl-lywycpcha-136[fwujm]
odiih-kdwwh-ldbcxvna-bnaerln-927[dnabh]
nuatmlmdpage-dmnnuf-xasuefuoe-586[atenm]
qxdwpopgsdjh-rpcsn-rdpixcv-htgkxrth-739[pkxqs]
iruzfrtkzmv-sleep-rthlzjzkzfe-633[ykopj]
ejpanjwpekjwh-nwxxep-zaoecj-550[ejpwa]
rwcnawjcrxwju-kdwwh-cnlqwxuxph-979[genou]
rwcnawjcrxwju-lqxlxujcn-mnyuxhvnwc-719[wxnmk]
qlm-pbzobq-avb-jxohbqfkd-601[mzylk]
lqwhuqdwlrqdo-vfdyhqjhu-kxqw-frqwdlqphqw-881[qwdhl]
udglrdfwlyh-gbh-ilqdqflqj-439[otyms]
hvbizodx-ezggtwzvi-nojmvbz-473[isoun]
dzczkrip-xiruv-vxx-ivtvzmzex-763[yblku]
oaxadrgx-bxmefuo-sdmee-xasuefuoe-222[exaou]
bnmrtldq-fqzcd-bzmcx-vnqjrgno-287[odhcx]
wifilzof-wbiwifuny-lyuwkocmcncih-864[iwcfl]
ynukcajey-ydkykhwpa-yqopkian-oanreya-654[cghze]
egdytrixat-qjccn-bpgztixcv-349[ctgix]
slqryzjc-cee-kypicrgle-210[gqknl]
jef-iushuj-rqiauj-iqbui-296[mzxhd]
qcffcgwjs-gqojsbusf-vibh-gozsg-220[gsfbc]
eadalsjq-yjsvw-usfvq-ugslafy-vwnwdghewfl-606[kyzev]
willimcpy-jfumncw-alumm-xymcah-318[mclai]
excdklvo-bkllsd-vklybkdybi-432[hrznt]
gntmfefwitzx-gzssd-xjwanhjx-801[xfgjn]
qzoggwtwsr-xszzmpsob-fsgsofqv-636[cthmf]
houngfgxjuay-hgyqkz-xkykgxin-332[spxuj]
oaxadrgx-eomhqzsqd-tgzf-qzsuzqqduzs-638[meluy]
qzoggwtwsr-qobrm-rsdzcmasbh-740[fwsan]
bkzrrhehdc-sno-rdbqds-qzaahs-vnqjrgno-339[rdhnq]
excdklvo-cmkfoxqob-rexd-zebmrkcsxq-458[xaqgb]
oknkvcta-itcfg-uecxgpigt-jwpv-fgukip-856[gcikp]
shoewudys-rkddo-qdqboiyi-530[doiqs]
mrxivrexmsrep-gerhc-pefsvexsvc-828[ubzia]
fydelmwp-mfyyj-hzcvdsza-769[anbml]
rwcnawjcrxwju-ljwmh-bqryyrwp-277[nxatm]".

day_6a_input() ->
  "cmezkqgn
nmzrgcft
ydpndcps
zjihhows
kvptxsrx
ubbvugwq
pclcquhl
rtddzpes
gfkylkvo
cpxpjjme
qqntjofm
tnvmqrik
cczmxxag
ikbrgpjh
lpeohbro
sgdidbgw
apjhovfs
miwqgpmr
igkccbxe
dcfpfkdv
neaxgnpr
xjlnhgwz
hbwdbtmt
jaahaztu
xdhkxiwj
kbcnydre
zygzcjxg
pnhlsbyu
gpkfcakg
vlpebsme
fhivcwnn
avscujyu
tckpnxnn
vhtaizda
vghhmhuy
dtzhrwcw
qhbcdaxx
kdoadrvh
yrjzipbd
weqfqmqr
zlkaiefc
zziwfitz
hfdvzpol
opialtmr
wgbarxig
gguytyxk
gwvaqisb
vybedyip
cbcdebwm
twoqbnis
itrspsmt
cqvjpfou
avhpvkbz
xozehrwd
qizmzubk
hpyiulwy
clmrwgdt
uruutjhx
pyvkmpxk
wpjfzzst
hjxjjkup
mdtlnvab
tqwnjufv
nlaxmbxc
nyetqfpn
nmapoequ
aozqvnbx
awuopxxj
jjamjzdr
xsgnpwrv
odpbdulf
nnpddykk
fwkqbeeq
rmpyqcrr
nnrbqymd
advolplo
xfwzojqb
dlxozmgp
mehtypai
qgxmpmza
cyflmzcf
drilfbik
hsrkwohm
lzdcksvs
xtqiuyon
aatvfuvn
tgdwdznm
srlndtlz
kcdtqqov
rjjwcfpr
sqmwnyjj
spfagdkw
ffqrocvz
fdncyaef
doymrkhy
nagivkzc
ylvmvlvo
yqnpiqnx
yqiuccji
swugswxs
wlfcvtms
bplwnlqh
dyqqbiop
ugxdfwnu
actfbdnl
hafvcdjm
uxlvddgb
jimpqraf
oovjqvmc
niixikhh
uamcczvl
iqyhtphk
hmgnaqfa
anptkatn
taslmdqh
hrsdlgth
tidxkojm
bozyplbl
viyiykes
bqttiowc
fdygoexj
yxiqrabo
hoqmzyap
qrdjlssb
kpoknmcl
wmfbbpoz
xyfmwzrc
ekgikzyt
furxwelu
gtfoyquj
xhtkpgnb
pqwfaoeh
kgutwopd
gmsrhxhp
yfriofga
kjulfqdc
anyrvwxv
reuufyff
rhhuhyku
muwxqimh
lmmesfgq
buklvija
nrqemlud
waggxokb
dmmtiifd
kgawgnsa
pvwrwdhz
mboaagdf
tugpycjc
yrrurffl
xnpptcxi
wynqznnj
pecxtzem
qsmjkvvd
gbosyfyx
dckxdlle
oyuucewm
rvzinbwp
bwdsapew
qacnmkst
dunstuov
gfrmztat
psehmndx
krhyzbag
trxayqjv
ddhrarzx
msnjiwaf
znjklkrs
gzhgcuqn
eoivvakl
ekjbelae
oxvbtsmk
mwfqyskr
tihtgxtf
hldkxeuc
nnawdxvy
euemeepz
ibnuhhex
ojwihmnv
cfpezewj
vrxjrwia
wgmyafnj
pnrsmxka
ksuwbzlt
uwkupngv
jdajpsal
tbufcuza
jjgptlxn
hxoulqig
gieqsttk
fwjyxnaq
pmfdifiq
qcgjfmsh
bnzqevtw
zlosluzk
pyfrslkb
ivzxjsgx
wahqmige
uhvsplzs
qaatujkd
taryjkox
jwdwisfv
dtwhlvuv
lwlwbjee
wopsiktn
iojihkrw
pwmqgwpk
kepvgmcd
dqgupbhg
srofdewh
ntijingz
osixtaku
isacbsnl
txtaxccj
uuqanmcw
nsuogfzt
yktybcsy
csqjvxog
rrjygfmc
eftdwemr
uxbswaep
zghswtrf
fhlxbray
julloyea
bsxwmvfv
kzatuvcu
mnymrdpq
idejsnhx
kdbpzapz
tzjefanj
ottzlwxh
mifokhqj
lxxbtzjr
wjcblnsd
siiozsqc
iujapalx
ofsvvyuy
zbgpxvrb
aqbilxlp
ncobthcc
sflihopk
pxwtiwam
nmgzdpyj
nhjhaezr
weihbqyp
pkpnbhxp
dlrelmop
mjbvnjuq
qntmdrey
htiluzbi
fingzxbe
mnekisyu
ynfcmhzd
vdzoljfg
wfmscpvw
efvyjhux
gvfkaxjq
rkmkahxl
vhqijllu
kkjpwxlq
londfadk
ohsxywdq
znstqcbb
qtazxfoi
jdqwiadz
mumicrid
uhwfytgm
srqofgqp
gtlqqspw
kxnkrcln
aycqjkay
yvangrcm
tpokdbwt
hmfqugbw
qoymvotr
icjendxu
uqsvumij
bqkqoeul
riarnbdv
zwlltddu
izcmngof
lawuhjjj
fdtnicju
iizykequ
lwrfolub
rknrbikc
yvogoydm
bogzdkiw
obnhuoxn
lzzpupsk
nuefyzzr
azghigtg
mkyduyug
mnteeioi
yhqbtwyx
eaojxpwy
hbbxehvr
omdkihmb
hbcijcio
settptzw
babyhhhe
cdlexgrs
cwrdtzjk
xvtwjacw
lxeykife
szogbxgb
ggxlgisl
kbmrnfro
ioervjsx
pfkodypz
ojgbokwc
jvykzhzc
cmigvhio
wwiowvyo
igwtrxhe
obawztja
yyazfxks
gfqqttue
czmvgttl
aljlhlyo
zczpqnzb
ruofwgrx
bhemgvlr
yzsulgck
eixzpfkh
cbejkdrs
qcsnnfht
ryvlmbiz
nfswleyf
xtoxoitk
ysfgwpmy
zsnapbrq
olqagygt
zmtyqfvd
ztybusgn
zsydzdnl
fkbvfvsq
gwdjudok
juzbnhfe
apivbufk
ozxgeeqa
yvyvuvxh
kexcesza
gqefjmed
hqyolehg
mluggzqh
gkpjfkhg
bmvxtrci
euyduveo
avwdogys
jnserfgo
iysfpsns
nxilicng
rpclnuwl
anxroxpu
fjmenahn
xngxqxxt
ziwltmcm
rdizrucj
wvvwldvq
blyiqvpw
iklfxllo
txueozfv
wapwemje
bztthavf
fkfejluf
iwynejes
mkwpylhy
pmndxgby
vhgdvrbv
fizshysy
phqddggq
bosaehqz
kwsoncrz
pmaethwo
valgeqbq
rcjuatfg
ryaujqvn
urpgwdyv
gdefrqbu
jcpfzans
eywcyjer
xpkacpyo
xqdukuff
lmbaxfqi
tzvnhfms
osqfwpss
ltgvoipl
bcorqrzk
wgccrykp
aaaoczvn
jpbsehyo
qtfzphwh
bpiiwzib
tnxbnwyg
xruheaca
eoxvahaq
dzhcleaw
vwcgptbp
mmqzjwte
gpxrndsm
kdgwktpb
roqqxgvt
tceymtaf
pkelkvvi
jqfguroe
xbrhyuai
jvbizlbh
hhujmghp
xxtagkzc
pxtzfvsy
vlopcrko
lorhgtfj
eyuzxpjt
jxjbdzrs
jfcuqypt
dcmbqqln
stdmubrl
fkvvwbue
mqqhkoqd
lvmnavnr
gtxksotd
dyjdydhj
rknodxpp
nkrbeqgp
lzzlxjub
hfhycqag
zrhtmjcz
tetkoiki
aeicawds
kvverwcb
vkkmanit
ozzoauql
eqjceipv
vjeajvzj
rfbyfkdt
ayudrwvi
ozlumnku
bbmgldja
dwpjacmb
ddyqbnzl
jlrdfzef
quovmsbh
utposqki
howsfhba
rdddsgwx
fcdtcqni
kbhnvmah
cgpbjquu
qjhmpyff
wxkytidy
ssefidnf
opswmrqz
zhcskfsp
hhkqbfon
uvgdhifc
eoewusji
xjmylrdx
fabeoujy
gzrceopo
fxsivztv
veqxwblf
sacoxlhm
xongcuef
lufmhuoi
juzgavxq
jjwlcfjq
egmnqjqn
ryhlipod
uagzcjur
epjngrwa
fijrzmww
zihnvpgp
zjurrctz
irhnbjjr
mlrfavaa
cokssyim
auwsrcsm
wrkkttyo
cmskryli
mrkpezgq
ehefyaqv
ivsuxdll
gscbkguh
bfxberbd
vihesdxg
vdbxzltv
lkoiranw
qcnefblb
cfftjwud
xqpieetw
crnrywvn
eepxytfc
cacfhgnf
bakhanwy
lsnlnmrj
usaurokx
sjqbyile
lvcgmrte
vesupotm
yeusftiz
clnjmcit
jhexzuyh
wtbiuozi
fsnqljcg
fxretbsa
lsagjnhx
jjknskzr
dllskstv
vgxhdbyw
yryqoqgz
ycilkokz
vfdcsamh
oedmwosl
vzwfymbu
eqrznqgp
fevhvwom
qextbmed
ubdsfkiu
stvuqrka
nmcrshqw
zlfzaxmw
qzcagqcq
djudatbg
usknomtt
busciicd
wyugburo
qblpvrxc
shzawivm
ztgzrklm
ahpxtdmz
obvuhnlj
uihsumey
mircsnyv
ijjhkyjw
dgxmzhgq
rqavgasa
lelkschr
svzzvroa
sevzfvbh
kgzcpbdj
wvctsjcp
kgdrxolj
tlsksbdi
ycqvhidx
epcaeqir
xcrgjgzi
snuvvmmy
cxbxoxvb
leykoxno
ppvysjob
eubrylie
pxspjeqg
xbdesmuq
bfcpktpy
elyounyn
niwhwuak
hukkheui
ueojrjoc
mktpkpsk
uxljxoei
hymwnsrf
sgyywcqt
yznoeeft
puvcmnpe
domsvurc
ukbhxndd
qwlzklcm
qttwpwdc
vxljmley
sjlbsszg
iqobsomn".
