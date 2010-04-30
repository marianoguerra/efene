-module(fn_shell).

-export([start/0]).

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
             _:Error -> io:format("~p~n", [Error]), loop(Bindings)
           end;
       true -> io:format("~n"), ok
    end.

