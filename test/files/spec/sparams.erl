-module(sparams).
-export([f_1/1, f_2/1, f_3/1, f_4/1, f_5/1, f_6/1]).

-spec f_1(maybe_improper_list(any(),  any())) -> maybe_improper_list(any(), any()).
f_1(List) ->
    List.


-spec f_2(list(integer())) -> list(integer()).
f_2(List) ->
    List.


-spec f_3(list(integer() | float())) -> list(integer() | float()).
f_3(List) ->
    List.


-spec f_4(Foo :: [char()]) -> list(char()).
f_4(List) ->
    List.


-spec f_5([[char()]]) -> list(list(char())).
f_5(List) ->
    List.


-spec f_6({atom(),atom(),byte()}) -> {atom(),atom(),byte()}.
f_6(Tuple) ->
    Tuple.
