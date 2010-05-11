-module(fn_errors).
-export([handle/1, fail_on_tab/1, format_error/1]).

format_error({error, {Line, fn, {illegal, tab}}}) ->
    io:format("line ~p: illegal tab found on ifene source (use spaces for indentation)~n", [Line]);
format_error({error, {Line, fn_lexer, {illegal, Char}}, _Unk}) ->
    io:format("line ~p: illegal character: \"~s\"~n", [Line, Char]);
format_error({error, {Line, fn_lexer, [Reason, Cause]}}) ->
    io:format("line ~p: ~s \"~s\"~n", [Line, Reason, Cause]);
format_error({error, {Line, fn_parser, [Reason, Cause]}}) ->
    io:format("line ~p: ~s \"~s\"~n", [Line, Reason, Cause]);
format_error(Error) ->
    io:format("Error: ~p~n", [Error]).

% throw an exception if a tab is found, otherwise return the Lex
fail_on_tab(Lex) ->
    fail_on_tab(Lex, []).

fail_on_tab([], Accum) ->
    lists:reverse(Accum);
fail_on_tab([{tab, Line, _}|_T], _Accum) ->
    throw({error, {Line, fn, {illegal, tab}}});
fail_on_tab([H|T], Accum) ->
    fail_on_tab(T, [H|Accum]).

handle(Function) ->
    try
        Function()
    catch _: Throw->
        format_error(Throw),
        erlang:halt()
    end.

