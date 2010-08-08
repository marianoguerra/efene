-module(fn_errors).
-export([handle/1, fail_on_tab/1, format_error/1]).

-license("New BSD License, part of efene, see LICENSE for details").

format_error(_, {error, {Line, fn, {illegal, tab}}}) ->
    io:format("line ~p: illegal tab found on ifene source (use spaces for indentation)~n", [Line]);
format_error(_, {error, {Line, fn_lexer, {illegal, Char}}, _Unk}) ->
    io:format("line ~p: illegal character: \"~s\"~n", [Line, Char]);
format_error(_, {error, {Line, fn_lexer, [Reason, Cause]}}) ->
    io:format("line ~p: ~p ~p~n", [Line, Reason, Cause]);
format_error(_, {error, {Line, fn_parser, [Reason, Cause]}}) ->
    io:format("line ~p: ~p ~p~n", [Line, Reason, Cause]);
format_error(_, {error, enoent}) ->
    io:format("Error: File doesn't exist~n");
format_error(Type, Error) ->
    Stacktrace = erlang:get_stacktrace(),
    PF = fun(Term, I1) -> pp(Term, I1) end,
    SF = fun(M, _F, _A) -> (M =:= fn_shell) or (M =:= erl_eval) or (M =:= init) end,
    io:format("~s~n", [lib:format_exception(1, Type, Error, Stacktrace, SF, PF)]).

format_error(Error) ->
    format_error(throw, Error).

pp(V, I) ->
    io_lib_pretty:print(V, I, 80, 30, 60).

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
    catch Type: Throw->
        format_error(Type, Throw),
        erlang:halt()
    end.

