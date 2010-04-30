-module(test_lexpp).
-compile(export_all).

test_indent_to_blocks() ->
    Test = fun (Path) ->
            Str = file_to_string(Path),
            io:format("~s~n", [Str]),
            Lex = fn:get_lex(istring, Str),
            io:format("~p~n", [Lex]),
            fn:pretty_print(Lex, true),
            fn:pretty_print(Lex, false)
    end,
    Test("../examples/argmatch.ifn"),
    Test("../examples/if_expr.ifn"),
    ok.

test_clean_whites() ->
    Test = fun (Path) ->
            Str = file_to_string(Path),
            io:format("~s~n", [Str]),
            io:format("~s~n", [Str]),
            Lex = fn:get_lex(string, Str),
            io:format("~p~n", [Lex]),
            fn:pretty_print(Lex, true),
            fn:pretty_print(Lex, false)
    end,
    Test("../examples/argmatch.fn"),
    Test("../examples/if_expr.fn"),
    ok.

all() ->
    test_clean_whites(),
    test_indent_to_blocks()
    .

file_to_string(Path) ->
    {ok, Content} = file:read_file(Path),
    binary_to_list(Content).
