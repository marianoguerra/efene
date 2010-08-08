-module(fn_shell).
-export([start/0]).

-license("New BSD License, part of efene, see LICENSE for details").

start() ->
    loop(erl_eval:new_bindings()).

loop(Bindings) ->
    Expression = io:get_line(">>> "),
    if Expression /= eof ->
           try Ast = fn:get_ast(string, Expression),
               {value, Result, NewBindings} = erl_eval:exprs(Ast,
                                                             Bindings),
               io:format("~p~n", [Result]),
               loop(NewBindings)
           catch
             _:Error -> fn_errors:format_error(Error), loop(Bindings)
           end;
       true -> io:format("~n"), ok
    end.

