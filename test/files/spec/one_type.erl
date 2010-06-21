-module(one_type).
-export([f_1/1, f_2/1, f_3/1, f_4/1, f_5/1, f_6/1, f_7/1, f_8/1]).
% test for common type
-spec f_1(integer()) -> integer().
f_1(Int) ->
    Int.

% test for an alias
-spec f_2(tuple()) -> tuple().
f_2(Tuple) ->
    Tuple.

% test for a literal
-spec f_3(4) -> 5.
f_3(4) ->
    5.

% test for an empty binary literal
-spec f_4(<<>>) -> <<>>.
f_4(<<>>) ->
    <<>>.

% test for an empty list literal
-spec f_5([]) -> [].
f_5([]) ->
    [].

% test for an empty tuple literal
-spec f_6({}) -> {}.
f_6({}) ->
    {}.

% test for list type
-spec f_7(list()) -> list().
f_7(List) ->
    List.

% test for atom literal
-spec f_8(some_atom) -> 'if'.
f_8(some_atom) ->
    'if'.
