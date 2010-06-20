-module(one_type).
-export([one_type_1/1, one_type_2/1]).

-spec one_type_1(integer()) -> integer().
one_type_1(Int) ->
    Int.


-spec one_type_2(tuple()) -> tuple().
one_type_2(Tuple) ->
    Tuple.

