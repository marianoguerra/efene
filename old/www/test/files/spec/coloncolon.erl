-module(coloncolon).
-export([f_1/1, f_2/1]).

-spec f_1(ArgName :: maybe_improper_list(any(),  any())) -> maybe_improper_list(any(), any()).
f_1(List) ->
    List.


-spec f_2(VarName :: foo() | bar()) -> foo() | bar().
f_2(Val) ->
    Val.
