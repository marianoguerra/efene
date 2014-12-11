-module(fn_error).
-export([to_string/2]).

to_string(Module, Errors) when is_list(Errors) ->
    lists:map(fun (Error) -> to_string(Module, Error) end, Errors);
to_string(Module, {Type, Line, Details}) ->
    TypeStr = type_to_string(Type),
    DetailsStr = details_to_string(Details),
    io_lib:format("~p:~p:~p: ~s at line ~p ~s~n", [Module, Line, Type, TypeStr, Line, DetailsStr]).

type_to_string(case_mismatch) -> <<"Case Mismatch">>;
type_to_string(invalid_export) -> <<"Invalid Export">>;
type_to_string(invalid_expression) -> <<"Invalid Expression">>;
type_to_string(invalid_catch) -> <<"Invalid Catch">>;
type_to_string(Other) -> atom_to_list(Other).

details_to_string({expected, Expected, got, Got}) ->
    io_lib:format("Expected ~p got ~p", [Expected, Got]);
details_to_string(Other) ->
    io_lib:format("~p", [Other]).
