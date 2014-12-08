-module(tu).
-compile(export_all).

% test utils

same(A, B, Verbose) ->
    if
        Verbose -> io:format("checking ~p = ~p~n", [A, B]);
        true -> ok
    end,

    try
        A = B,
        ok
    catch
        error:_Error -> {error, A, B}
    end.


same_ast(FnExpr, ErlExpr) ->
    FnAst = fn:get_ast(string, FnExpr ++ ";"),
    ErlAst = fn:erl_to_ast(ErlExpr ++ "."),
    try
        FnAst = ErlAst,
        ok
    catch
        error:_Error -> {error, FnAst, ErlAst}
    end.

same_mod_ast(FnMod, ErlMod, ModName) ->
    FnAst  = fn:mod_to_ast(FnMod, ModName ++ ".fn"),
    ErlAst = fn:erlmod_to_ast(ErlMod, ModName ++ ".erl"),
    try
        FnAst = ErlAst,
        ok
    catch
        error:_Error -> {error, FnAst, ErlAst}
    end.

same_ast_no_endl(FnExpr, ErlExpr) ->
    FnAst = fn:get_ast(string, FnExpr),
    ErlAst = fn:erl_to_ast(ErlExpr),
    try
        FnAst = ErlAst,
        ok
    catch
        error:_Error -> {error, FnAst, ErlAst}
    end.

same_file_ast(FnPath, ErlPath) ->
    [_|T] = fn:from_erlang(ErlPath),
    ErlAst = lists:reverse(tl(lists:reverse(T))),

    FnAst = fn:build_module(FnPath),

    try
        FnAst = ErlAst,
        ok
    catch
        error:_Error -> {error, FnAst, ErlAst}
    end.

same_file_ast_fn(FnPath1, FnPath2) ->
    [_|FnAst1] = fn:build_module(FnPath1),
    [_|FnAst2] = fn:build_module(FnPath2),

    try
        FnAst1 = FnAst2,
        ok
    catch
        error:_Error -> {error, FnAst1, FnAst2}
    end.

test_ast(FnExpr, ErlExpr) ->
    test_ast(FnExpr, ErlExpr, same_ast).

test_ast(FnExpr, ErlExpr, Fun) ->
    Result = try
        ?MODULE:Fun(FnExpr, ErlExpr)
    catch
        _:Err -> {error, Err}
    end,

    case Result of
        ok -> io:format("ok: ~p = ~p~n", [FnExpr, ErlExpr]);
        {error, Error} -> io:format("error: ~p = ~p~n    ~p~n", [FnExpr, ErlExpr, Error]);
        {error, FnAst, ErlAst} -> io:format("error: ~p = ~p~n~p~n~p~n",
                [FnExpr, ErlExpr, FnAst, ErlAst])
   end.

test_file(FnPath, ErlPath) ->
    case same_file_ast(FnPath, ErlPath) of
        ok ->
            io:format("ok: ~p = ~p~n", [FnPath, ErlPath]);
        {error, _FnAst, _ErlAst} ->
            io:format("error: ~p = ~p~n", [FnPath, ErlPath])
    end.

test_file_fn(FnPath1, FnPath2) ->
    case same_file_ast_fn(FnPath1, FnPath2) of
        ok ->
            io:format("ok: ~p = ~p~n", [FnPath1, FnPath2]);
        {error, _FnAst1, _FnAst2} ->
            io:format("error: ~p = ~p~n", [FnPath1, FnPath2])
    end.

test_mod_ast(FnMod, ErlMod, ModName) ->
    Result = try
        same_mod_ast(FnMod, ErlMod, ModName)
    catch
        _:Err -> {error, Err}
    end,

    case Result of
        ok -> io:format("ok: ~p = ~p~n", [FnMod, ErlMod]);
        {error, Error} -> io:format("error: ~p = ~p~n    ~p~n", [FnMod, ErlMod, Error]);
        {error, FnAst, ErlAst} -> io:format("error: ~p = ~p~n~p~n~p~n",
                [FnMod, ErlMod, FnAst, ErlAst])
   end.

test(Module) ->
    io:format("~ntesting ~p~n", [Module]),
    Module:all().

test(Module, Function) ->
    io:format("~ntesting ~p:~p~n", [Module, Function]),
    Module:Function().
