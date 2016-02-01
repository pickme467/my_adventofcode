-module(day_1).

-export([run/0, count_brackets/2]).

run() ->
  reduce(map(input())).

map(List) ->
  map(List, 0).

map([], Count) ->
  Count;
map([H | T], Count) ->
  spawn(day_1, count_brackets, [self(), H]),
  map(T, Count + 1).

reduce(Count) ->
  reduce(Count, 0).

reduce(0, Sum) ->
  Sum;
reduce(Count, Sum) ->
  receive Value ->
      reduce(Count - 1, Sum + Value)
  end.

count_brackets(Pid, List) ->
  count_brackets(Pid, List, 0).

count_brackets(Pid, [], Number) ->
  Pid ! Number;
count_brackets(Pid, [$(  | Tail], Number) ->
  count_brackets(Pid, Tail, Number + 1);
count_brackets(Pid, [$)  | Tail], Number) ->
  count_brackets(Pid, Tail, Number - 1).

input() ->
  ["()(((()))(()()()((((()(((())(()(()((((((()(()(((())))((()(((()))((())(()("
   "(()()()()(((())(((((((())))()()(()(()(())(((((()()()((())(((((()()))))()("
   "())(((())(())((((((())())))(()())))()))))()())()())((()()((()()()()(()((("
   "(((((()()())((()()(((((()(((())((())(()))()((((()((((((((())()((()())(())"
   "((()))())((((()())(((((((((((()()(((((()(()))())(((()(()))())((()(()())()"
   ")())(()(((())(())())()()(()(()((()))((()))))((((()(((()))))((((()(()(()()"
   ")())()(((()((((())((((()(((()()(())()()()())((()((((((()((()()))()((()))("
   ")(()()((())))(((()(((()))((()((()(()))(((()()(()(()()()))))()()(((()(((()"
   ")())))))((()(((())()(()(())((()())))((((())))(()(()(()())()((()())))(((()"
   "((()(())()()((()((())(()()((())(())()))()))((()(())()))())(((((((()(()()("
   "()(())())))))))(()((((((())((((())((())())(()()))))()(())(()())()())((())"
   "(()))))(()))(()((()))()(()((((((()()()()((((((((()(()(())((()()(()()))(()"
   ")()())()((())))()))()())(((()))(())()(())()))()((()((()(()()())(())()()()"
   "((())())))((()()(()()((()(())()()())(((()(()()))))(())))(()(()())()))()()"
   "))))))()))))((((((())))())))(()(())())(()())))))(()))()))))))()((()))))()"
   "))))(()(()((()())())(()()))))(((())()))())())())(((()(()()))(())()(())(()"
   ")((((((()()))))((()(()))))))(()))())(((()()(()))()())()()()()))))))))))))"
   ")(())(()))(()))((()(())(()())(())())(()())(())()()(()())))()()()))(())())"
   "()))())())(())((())))))))(())))(())))))()))))((())(()(((()))))(()))()((()"
   "(())))(()())(((((()))()())()()))))()))))()))())(()(()()()))()))))))((()))",
   "))))))))()((()))((()(())((())()()(()()))()(()))))()()(()))()))(((())))(()"
   ")()((())(())(()())()())())))))))())))()((())))()))(()))()()))(((((((()))("
   "))(()()))(()()(()))()(()((()())()))))))(((()()()())))(())()))()())(()()))"
   "()()))))))))(())))()))()()))))))()))()())))()(())(())))))()(())()()(()())"
   ")))())((()))))()))))(()(((((()))))))))())))())()(())()()))))(())))())()()"
   "())()()())()(()))))()))()))))))))())))((()))()))()))())))()())()()())))()"
   ")))(()((())()((()))())))))())()(())((())))))))))))())()())(())())())(()))"
   "(()))()))())(()(())())()())()()(()))))(()(())))))))(())))())(())))))))())"
   "()()(())())())))(())))))()))()(()())()(()))())())))))()()(()))()))))())))"
   "))))))()))))()))))))())()())()()))))()())))())))))))))))()()))))()()(((()"
   "))()()(())()))))((()))))(()))(())())))(())()))))))(()))()))))(())())))))("
   ")))(()())))))))))))))())))))))))()((()())(()())))))))((()))))(())(())))()"
   "(()())())))())())(()()()())))()))))))())))))())()()())))))))))))()()(()))"
   "))()())()))((()())(()))))()(()))))))))))()())())(((())(()))))())()))()))("
   ")))))))()))))))(()))))()))))()(())))(())))(()))())()()(()()))()))(()())))"
   ")))))()))(()))())(()()(()(()())()()))()))))))))(())))))((()()(()))())()))"
   ")))()))())(()())()()))())))()(()()()()))((())())))())()(()()))()))))))))("
   "()))(())))()))))(()(()())(()))))()())())()))()()))())))))))))))())())))))"
   ")()))))))))())))))()))))())(()())))(())()))())())))))()()(()()())(()())))"
   "()()))(((()))(()()()))))()))))()))))((())))()((((((()()))))))()))))))))))",
   ")(((()))))))))))))(())())))))())(()))))))(()))((()))())))()(()((()))()))("
   ")))))))))))())()))()(()()))))())))())(())()(()))()))())(()))()))))(()()))"
   "()()(())))))()))(())(()(()()))(()()())))))(((()))))))()))))))))))))(())(("
   ")))))()())())()()((()()))())))))(()))))())))))))()()()))))))))())))()(((("
   ")()))(())))))(((())())))))((()))()(()))(()))))(()())))(()))())))))()))))("
   "())(())))()((()))(())())))()()))()))))))))()))(()()()(()()()(()))())(())("
   ")())(((()))(())))))))))(((()())))()()))))))))()(())(()))()((((())(())(()("
   "))))()))(((())()()()))((()))(()))())())))())))(()))())()())())(()(())())("
   ")()()(())))())(())))(())))(())()))()))(()((()))))))))())(()))))))())(()()"
   "))()()))()(()(()())))()()(()((()((((((()))(())))()()()))())()))((()()(())"
   ")())((()(()(()))(()()))))()())))()))()())))))))()()((()())(())))()))(()))"
   "(())(()))())(()(())))()()))))))(((()(((()()))()(()(())())((()()))()))()))"
   "()))()(()()()(()))((()())()(())))()()))(((())()()())(())()((()()()()(()(("
   "))(()()))()(((((()())))((())))))(()()()))))(((()(())))()))((()((()(())()("
   "()((())))((()())()(()))(((()())()()(()))(())(((()((()())()((())()())(((()"
   "()))((()((())(()))(()())(()()()))((()))(())(()((()()())((()))(())))(())(("
   "))(())))(()())))(((((()(()(((((()())((((()(()())(())(()()(((())((()(((()("
   ")(((()()((((((())))())(()((((((()(()))()))()()((()((()))))()(()()(()((()("
   ")))))))(((((()(((((())()()()(())())))))))()))((()()(())))(())(()()()())))"
   "))(()((((())))))))()()(((()(()(()(()(()())()()()(((((((((()()())()(()))((",
   "()()()()()(((((((()())()((())()))((((((()(()(()(()())(((()(((((((()(((())"
   "(((((((((())(())())()))((()(()))(((()()())(())(()(()()(((()(())()))())))("
   "())((((((())(()()())()()(((()(((())(()(((())(((((((()(((((((((()))(())(()"
   "(()(()))))((()))()(())())())((()(()((()()))((()()((()(())(())(()((())(((("
   "))(((()()()((((((()()(())((((())()))))(())((()(()((())))(((((()(()()())()"
   ")((())())))((())((()((()()((((((())(((()()(()())())(()(()))(()(()))())())"
   "()(((((((()(((()(())()()((())((()(()()((()(()()(((((((((((())((())((((((("
   "))((()((((()(()((((()(((((((())()((()))))())()((()((((()(()(((()((()())))"
   "(())())(((()(((())((((((()(((((((((()()(())))(()(((((()((((()())))((()((("
   ")((()(()()(((())((((((((((((()(((())(()(((((()))(()()(()()()()()()((())(("
   "(((((())(((((())))))())()(()()(()(()(((()()(((((())(()((()((()(((()()((()"
   "((((())()))()((((())(())))()())(((())(())(()()((()(((()()((((((((((()()(("
   ")())())(((((((((())((((()))()()((((())(()((((()(((())())(((((((((((()(((("
   "())))(())(()(((()(((()((())(((((()((()()(()(()()((((((()((((()((()(()((()"
   "(()((((((()))))()()(((((()((()(()(())()))(())(((((((()((((()())(()((()((("
   ")(()))())))(())((()))))(((((((()()()())(()))(()()((()())()((()((()()()(()"
   "(()()))(()())(())(((((()(((((((((((()((()(((()(((((((()()((((((()(((((()("
   "()((()(((((())((((((()))((((())((()()((())(((())()(((((()()(((((()((()(()"
   "(((((((()(((((()((()((()((())(())((())(()))()()))(()()(()(()()(((((((()(("
   "(()(((())()(((((()((((((()())((((())()((()((()(()()())(()))((((()()((((((",
   "()((()(()(()((((()((()((())((((((()(()(())((((((()((((((((((()((())()))()"
   "(()(()(((((()()()))((())))()(()((((((((((((((()(((()((((()((())((()((()(("
   "(()()(()(((()((())(()()())))()(()(()(((((()()(()(()((((()(((((())()(()(()"
   "))(((((()()(((()()(())((((((((((((((())((())(((((((((((())()()()(())()(()"
   "(()(((((((((())(((()))(()()())(()((((()(())(((((()())(())((((((((())()((("
   "(()((((((())(()((()(())(((()((((()))(((((((((()()))((((()(())()()()(())(("
   ")((())((()()))()(((())(((((())((((((()()))(((((((((()((((((())))(((((((()"
   "((()(()(())))())(()(()))()(((((()())(()))()(()(())(((()))))())()())))(((("
   "(()))())()((()(()))))((()()()((((((()))()()((((((((())((()(()(((()(()((()"
   ")((()())(()((((())(()(((()()()(()(()()))())())((((((((((())())((()))()((("
   "))(())(())))())()(()()(())))())(()))(((()(()()(((()(((())))()(((()(())()("
   "(((((())()))()))()((((((()(()(((((()())))()))))())()()(((()(((((())((()()"
   "(()((()((()(()(()(())))(()()()()((()(())(((()((()))((((()))())(())))())(("
   ")))()()()())()))(((()()())()((())))(())(()()()()(()())((()(()()((((())))("
   "(()((()(())((()(()((())()(()()(((()())()()())((()))((())(((()()(())))()()"
   "))(((()((())()(((((()())(())((())()())())((((((()(()(((((()))(()("].