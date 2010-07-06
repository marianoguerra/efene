-module(range).
-export([f_1/1, f_2/1]).

-spec f_1(0..32) -> 0..32.
f_1(List) ->
    List.


-spec f_2(0..10 | 0..10) -> 1..2 | 2..2.
f_2(Val) ->
    Val.
