-module(fn_shell).
-export([start/0]).

-license("New BSD License, part of efene, see LICENSE for details").

start() ->
    loop(erl_eval:new_bindings()).

loop(Bindings) ->
    OriginalExpression = io:get_line(">>> "),

    if
        OriginalExpression /= eof ->
            Expression = ltrim(OriginalExpression),
            [FirstChar|_] = Expression,

            if FirstChar == $#; Expression == "\n" ->
                loop(Bindings);
            true ->
                try
                    Ast = fn:get_ast(string, Expression),
                    {value, Result, NewBindings} = erl_eval:exprs(Ast, Bindings),
                    fio:println(Result),
                    loop(NewBindings)
                catch
                    _:Error -> fn_errors:format_error(Error), loop(Bindings)
                end
            end;

        true -> io:format("~n"), ok
    end.

ltrim([32|Rest]) ->
    ltrim(Rest);

ltrim(Result) ->
    Result.
