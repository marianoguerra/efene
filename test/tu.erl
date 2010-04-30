-module(tu).
-compile(export_all).

% test utils

same_ast(FnExpr, ErlExpr) ->
    FnAst = fn:get_ast(string, FnExpr ++ ";"),
    ErlAst = fn:erl_to_ast(ErlExpr ++ "."),
    try
        FnAst = ErlAst,
        ok
    catch
        error:_Error -> {error, FnAst, ErlAst}
    end.

test_ast(FnExpr, ErlExpr) ->
    Result = try
        same_ast(FnExpr, ErlExpr)
    catch
        _:Err -> {error, Err}
    end,

    case Result of
        ok -> io:format("ok: ~s = ~s~n", [FnExpr, ErlExpr]);
        {error, Error} -> io:format("error: ~s = ~s~n    ~p~n", [FnExpr, ErlExpr, Error]);
        {error, FnAst, ErlAst} -> io:format("error: ~s = ~s~n~p~n~p~n",
                [FnExpr, ErlExpr, FnAst, ErlAst])
   end.

test(Module) ->
    io:format("~ntesting ~p~n", [Module]),
    Module:all().

test(Module, Function) ->
    io:format("~ntesting ~p:~p~n", [Module, Function]),
    Module:Function().
