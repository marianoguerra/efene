-module(one_type).
-export([one_type_1/1, one_type_2/1, one_type_3/1, one_type_4/1, one_type_5/1]).
% test for common type
-spec one_type_1(integer()) -> integer().
one_type_1(Int) ->
    Int.

% test for an alias
-spec one_type_2(tuple()) -> tuple().
one_type_2(Tuple) ->
    Tuple.

% test for a literal
-spec one_type_3(4) -> 5.
one_type_3(4) ->
    5.

% test for an empty binary literal
-spec one_type_4(<<>>) -> <<>>.
one_type_4(<<>>) ->
    <<>>.

% test for an empty list literal
-spec one_type_5([]) -> [].
one_type_5([]) ->
    [].

