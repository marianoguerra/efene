-module(fn_shell).
-compile(export_all).

start() ->
    fn_info:print_banner(),
    loop(erl_eval:new_bindings()).

loop(Bindings) ->
    Expression = io:get_line(">>> "),

    if
        Expression /= eof ->
            Ast = efene:get_ast(string, Expression),
            try
                {value, Result, NewBindings} = erl_eval:exprs(Ast, Bindings),
                io:format("~p~n", [Result]),
                loop(NewBindings)
            catch _:Error ->
                io:format("~p~n", [Error]),
                loop(Bindings)
            end;
        true ->
            io:format("~n"),
            ok
    end.


