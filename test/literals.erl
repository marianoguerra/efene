-module(literals).
-compile(export_all).

atoms() ->
    tu:test_ast("foo", "foo"),
    tu:test_ast("'foo'", "'foo'"),
    tu:test_ast("fun", "'fun'"),
    tu:test_ast("a@b", "a@b"),
    tu:test_ast("a1", "a1"),
    tu:test_ast("'!$%&'", "'!$%&'").

integers() ->
    tu:test_ast("1", "1"),
    tu:test_ast("0b1", "1"),
    tu:test_ast("0x1", "1"),
    tu:test_ast("0o1", "1"),
    tu:test_ast("16", "16"),
    tu:test_ast("0b10000", "16"),
    tu:test_ast("0x10", "16"),
    tu:test_ast("0o20", "16").

floats() ->
    tu:test_ast("1.0", "1.0"),
    tu:test_ast("1.10", "1.10"),
    tu:test_ast("123.10", "123.10"),
    tu:test_ast("123.10e2", "123.10e2"),
    tu:test_ast("123.10e-2", "123.10e-2"),
    tu:test_ast("123.10e+2", "123.10e+2"),
    tu:test_ast("123.10E2", "123.10E2"),
    tu:test_ast("123.10E-2", "123.10E-2"),
    tu:test_ast("123.10E+2", "123.10E+2").

bools() ->
    tu:test_ast("true", "true"),
    tu:test_ast("false", "false").

strings() ->
    tu:test_ast("\"\"", "\"\""),
    tu:test_ast("\"a\"", "\"a\""),
    tu:test_ast("\"abc\"", "\"abc\""),
    tu:test_ast("\"123\"", "\"123\""),
    tu:test_ast("\"!$%&\"", "\"!$%&\""),
    tu:test_ast("\"\\\"hi!\\\"\"", "\"\\\"hi!\\\"\"").

calls() ->
    tu:test_ast("a()", "a()"),
    tu:test_ast("A()", "A()"),
    tu:test_ast("a.b()", "a:b()"),
    tu:test_ast("a.B()", "a:B()"),
    tu:test_ast("A.B()", "A:B()"),
    tu:test_ast("A.b()", "A:b()"),
    tu:test_ast("A()()", "(A())()").

chars() ->
    tu:test_ast("$a", "$a"),
    tu:test_ast("$\\(", "$("),
    tu:test_ast("$\\n", "$\\n").

lists() ->
    tu:test_ast("[]", "[]"),
    tu:test_ast("[1:2]", "[1|2]"),
    tu:test_ast("[1:[2, 3]]", "[1|[2, 3]]"),
    tu:test_ast("[1:[2:3]]", "[1|[2|3]]"),
    tu:test_ast("[true]", "[true]"),
    tu:test_ast("[\"asd\"]", "[\"asd\"]"),
    tu:test_ast("[1]", "[1]"),
    tu:test_ast("[1, 2]", "[1, 2]"),
    tu:test_ast("[1, 2, 3]", "[1, 2, 3]"),
    tu:test_ast("[[]:[]]", "[[]|[]]"),
    tu:test_ast("[[1]:[]]", "[[1]|[]]"),
    tu:test_ast("[[1, 2]:[]]", "[[1, 2]|[]]"),
    tu:test_ast("[[1]:[1, atom]]", "[[1]|[1, atom]]"),
    tu:test_ast("[[1, false]:[1, atom]]", "[[1, false]|[1, atom]]"),
    tu:test_ast("[1:[]]", "[1|[]]"),
    tu:test_ast("[1:[4]]", "[1|[4]]"),
    tu:test_ast("[1:[4, 5]]", "[1|[4, 5]]"),
    tu:test_ast("[1:[4, 5, 6]]", "[1|[4, 5, 6]]"),
    tu:test_ast("[1, 2:[4, 5, 6]]", "[1, 2|[4, 5, 6]]"),
    tu:test_ast("[1, 2, 3:[4, 5, 6]]", "[1, 2, 3|[4, 5, 6]]"),
    tu:test_ast("[a, a, 3, false]", "[a, a, 3, false]"),
    tu:test_ast("[true, \"hi\", atom, 1, 1.0, $a]", "[true, \"hi\", atom, 1, 1.0, $a]"),
    tu:test_ast("[[]]", "[[]]"),
    tu:test_ast("[[[]]]", "[[[]]]"),
    tu:test_ast("[[], []]", "[[], []]"),
    tu:test_ast("[1, []]", "[1, []]"),
    tu:test_ast("[1, [1, 2]]", "[1, [1, 2]]"),
    tu:test_ast("[1, [1, 2, [1, 2]]]", "[1, [1, 2, [1, 2]]]").

tuples() ->
    tu:test_ast("(,)", "{}"),
    tu:test_ast("(1,)", "{1}"),
    tu:test_ast("(1, 2)", "{1, 2}"),
    tu:test_ast("(1, 2, 3)", "{1, 2, 3}"),
    tu:test_ast("(true, \"hi\", atom, 1, 1.0, $a)", "{true, \"hi\", atom, 1, 1.0, $a}"),
    tu:test_ast("((,),)", "{{}}"),
    tu:test_ast("((,), (,))", "{{}, {}}"),
    tu:test_ast("(1, (,))", "{1, {}}"),
    tu:test_ast("(1, (1, 2))", "{1, {1, 2}}"),
    tu:test_ast("(1, (1, 2, (1, 2)))", "{1, {1, 2, {1, 2}}}").

farity() ->
    tu:test_ast("fn a:0", "fun a/0"),
    tu:test_ast("fn a.b:0", "fun a:b/0"),
    tu:test_ast("fn a:1", "fun a/1"),
    tu:test_ast("fn a.b:1", "fun a:b/1").

list_comp() ->
    tu:test_ast("[A for A in B]", "[A || A <- B]"),
    tu:test_ast("[A for A in B if A > 0]", "[A || A <- B, A > 0]"),
    tu:test_ast("[(A, C) for A in B for C in D]", "[{A, C} || A <- B, C <- D]"),
    tu:test_ast("[(A, C) for A in B if A > 0 for C in D if C > A]",
        "[{A, C} || A <- B, A > 0, C <- D, C > A]").

bin_comp() ->
    tu:test_ast("<[ <[A]> for <[A]> <- B]>", "<< <<A>> || <<A>> <= B>>"),
    tu:test_ast("<[ <[(X + Y)]> for <[X]> <- <[1,2,3,4]> for <[Y]> <- <[1,2]> ]>",
        "<< <<(X+Y)>> || <<X>> <= <<1,2,3,4>>, <<Y>> <= <<1,2>> >>"),
    ok.

records() ->
    tu:test_ast("a.B[c]", "B#a.c"),
    tu:test_ast("a[,]", "#a{}"),
    tu:test_ast("a[b=1]", "#a{b=1}"),
    tu:test_ast("a[b=1, c=2]", "#a{b=1, c=2}"),
    tu:test_ast("a.B[]", "B#a{}"),
    tu:test_ast("a.B[b=1]", "B#a{b=1}"),
    tu:test_ast("a.B[b=1, c=2]", "B#a{b=1, c=2}").

binaries() ->
    tu:test_ast("<[1]>", "<<1>>"),
    tu:test_ast("<[1, 2]>", "<<1, 2>>"),
    tu:test_ast("<[1, 2, 3]>", "<<1, 2, 3>>"),
    tu:test_ast("<[\"mariano\"]>", "<<\"mariano\">>"),
    tu:test_ast("<[$a, $b, 1]>", "<<$a, $b, 1>>"),
    tu:test_ast("<[1024/utf8]>", "<<1024/utf8>>"),
    tu:test_ast("<[X:4/little-signed-integer-unit:8]>",
        "<<X:4/little-signed-integer-unit:8>>"),
    tu:test_ast("<[X:4/little-signed-integer-unit:8, Y:4/little-unsigned-unit:8]>",
        "<<X:4/little-signed-integer-unit:8, Y:4/little-unsigned-unit:8>>").

all() ->
    tu:test(?MODULE, atoms),
    tu:test(?MODULE, integers),
    tu:test(?MODULE, floats),
    tu:test(?MODULE, bools),
    tu:test(?MODULE, strings),
    tu:test(?MODULE, calls),
    tu:test(?MODULE, chars),
    tu:test(?MODULE, lists),
    tu:test(?MODULE, tuples),
    tu:test(?MODULE, farity),
    tu:test(?MODULE, list_comp),
    tu:test(?MODULE, bin_comp),
    tu:test(?MODULE, records),
    tu:test(?MODULE, binaries),
    ok.

