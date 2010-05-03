-module(fn_errors).
-export([handle/1]).

format_error({error, {Line, fn_lexer, {illegal, Char}}, _Unk}) ->
    io:format("line ~p: illegal character: \"~s\"~n", [Line, Char]);
format_error({error, {Line, fn_parser, [Reason, Cause]}}) ->
    io:format("line ~p: ~s \"~s\"~n", [Line, Reason, Cause]);
format_error(Error) ->
    io:format("Error: ~p~n", [Error]).

handle(Function) ->
    try
        Function()
    catch _: Throw->
        format_error(Throw),
        erlang:halt()
    end.

