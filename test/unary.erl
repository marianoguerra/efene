-module(unary).
-compile(export_all).

bool() ->
    tu:test_ast("not true", "not true"),
    tu:test_ast("not false", "not false"),
    tu:test_ast("not A", "not A"),
    tu:test_ast("not A()", "not A()"),
    tu:test_ast("not (true)", "not (true)"),
    ok.

decimal() ->
    tu:test_ast("+1", "+1"),
    tu:test_ast("-1", "-1"),
    tu:test_ast("+1.2", "+1.2"),
    tu:test_ast("-1.2", "-1.2"),
    tu:test_ast("+A", "+A"),
    tu:test_ast("-A", "-A"),
    tu:test_ast("+(1)", "+(1)"),
    tu:test_ast("-(1)", "-(1)"),
    tu:test_ast("+(A)", "+(A)"),
    tu:test_ast("-(A)", "-(A)"),
    tu:test_ast("-A()", "-A()"),
    tu:test_ast("+A()", "+A()"),
    ok.

binary() ->
    tu:test_ast("~1", "bnot 1"),
    tu:test_ast("~A", "bnot A"),
    tu:test_ast("~(1)", "bnot (1)"),
    tu:test_ast("~(A)", "bnot (A)"),
    tu:test_ast("~A()", "bnot A()"),
    ok.

all() ->
    tu:test(?MODULE, bool),
    tu:test(?MODULE, decimal),
    tu:test(?MODULE, binary),
    ok.
