-module(exprs).
-compile(export_all).

send() ->
    tu:test_ast("a ! b", "a ! b"),
    tu:test_ast("a ! b ! c", "a ! b ! c"),
    tu:test_ast("(a, 1) ! b ! c", "{a, 1} ! b ! c"),
    ok.

match() ->
    tu:test_ast("(a = b)", "a = b"),
    tu:test_ast("(a = b = c)", "a = b = c"),
    tu:test_ast("A = B", "A = B"),
    tu:test_ast("A = B = C", "A = B = C"),
    tu:test_ast("A = B = C = 1", "A = B = C = 1"),
    ok.

bool() ->
    tu:test_ast("true and false", "true andalso false"),
    tu:test_ast("true or false", "true orelse false"),
    tu:test_ast("true xor false", "true xor false"),
    tu:test_ast("true and false or true", "true andalso false orelse true"),

    tu:test_ast("true andd false", "true and false"),
    tu:test_ast("true orr false", "true or false"),
    tu:test_ast("true andd false orr true", "true and false or true"),
    tu:test_ast("true andd false orr not true orr true andd false",
        "true and false or not true or true and false"),

    tu:test_ast("true and not false", "true andalso not false"),
    tu:test_ast("not true or false", "not true orelse false"),
    tu:test_ast("true and false or not true", "true andalso false orelse not true"),
    tu:test_ast("true and false or not true or true and false",
        "true andalso false orelse not true orelse true andalso false"),
    ok.

comp() ->
    tu:test_ast("1 > 1", "1 > 1"),
    tu:test_ast("1 < 1", "1 < 1"),
    tu:test_ast("1 >= 1", "1 >= 1"),
    tu:test_ast("1 <= 1", "1 =< 1"),
    tu:test_ast("1 == 1", "1 == 1"),
    tu:test_ast("1 === 1", "1 =:= 1"),
    tu:test_ast("1 != 1", "1 /= 1"),
    tu:test_ast("1 !== 1", "1 =/= 1"),
    tu:test_ast("1 > 1 and 2 < 2", "1 > 1 andalso 2 < 2"),
    tu:test_ast("1 > 1 and 2 < 2 or 3 == 3 and 4 != 4",
        "1 > 1 andalso 2 < 2 orelse 3 == 3 andalso 4 /= 4"),
    tu:test_ast("(1 > 1) andd (2 < 2)", "(1 > 1) and (2 < 2)"),
    tu:test_ast("1 > 1 or 2 < 2", "1 > 1 orelse 2 < 2"),
    tu:test_ast("(1 > 1) orr (2 < 2)", "(1 > 1) or (2 < 2)"),
    tu:test_ast("(1 > 1) andd (2 < 2) orr (3 == 3) andd (4 != 4)",
        "(1 > 1) and (2 < 2) or (3 == 3) and (4 /= 4)"),
    ok.

concat() ->
    tu:test_ast("[] ++ []", "[] ++ []"),
    tu:test_ast("A ++ []", "A ++ []"),
    tu:test_ast("A ++ B", "A ++ B"),
    tu:test_ast("[1] ++ [2] ++ [3]", "[1] ++ [2] ++ [3]"),
    tu:test_ast("[1, [a, b] ++ [c, d]] ++ [2] ++ [3]",
        "[1, [a, b] ++ [c, d]] ++ [2] ++ [3]"),

    tu:test_ast("[] -- []", "[] -- []"),
    tu:test_ast("A -- []", "A -- []"),
    tu:test_ast("A -- B", "A -- B"),
    tu:test_ast("[1] -- [2] -- [3]", "[1] -- [2] -- [3]"),
    tu:test_ast("[1, [a, b] -- [c, d]] -- [2] -- [3]",
        "[1, [a, b] -- [c, d]] -- [2] -- [3]"),
    ok.

add() ->
    tu:test_ast("1 + 2", "1 + 2"),
    tu:test_ast("1 + 2 + 3", "1 + 2 + 3"),
    tu:test_ast("1 - 2", "1 - 2"),
    tu:test_ast("1 - 2 - 3", "1 - 2 - 3"),
    tu:test_ast("1 | 2", "1 bor 2"),
    tu:test_ast("1 | 2 | 3", "1 bor 2 bor 3"),
    tu:test_ast("1 ^ 2", "1 bxor 2"),
    tu:test_ast("1 ^ 2 ^ 3", "1 bxor 2 bxor 3"),

    tu:test_ast("1 + 2 - 3 | 4 ^ 5 | 4 - 3 + 2",
        "1 + 2 - 3 bor 4 bxor 5 bor 4 - 3 + 2"),
    ok.

mul() ->
    tu:test_ast("1 * 2", "1 * 2"),
    tu:test_ast("1 * 2 * 3", "1 * 2 * 3"),
    tu:test_ast("1 / 2", "1 / 2"),
    tu:test_ast("1 / 2 / 3", "1 / 2 / 3"),
    tu:test_ast("1 // 2", "1 div 2"),
    tu:test_ast("1 // 2 // 3", "1 div 2 div 3"),
    tu:test_ast("1 % 2", "1 rem 2"),
    tu:test_ast("1 % 2 % 3", "1 rem 2 rem 3"),
    tu:test_ast("1 & 2", "1 band 2"),
    tu:test_ast("1 & 2 & 3", "1 band 2 band 3"),

    tu:test_ast("1 * 2 / 3 % 4 // 5 & 5 % 4 / 3 * 2",
        "1 * 2 / 3 rem 4 div 5 band 5 rem 4 / 3 * 2"),
    ok.

random() ->
    tu:test_ast("1 + 2 - 3 | 4 ^ 5 | 4 - 3 + 2 + 1 * 2 / 3 % 4 & 5 % 4 / 3 * 2",
        "1 + 2 - 3 bor 4 bxor 5 bor 4 - 3 + 2 + 1 * 2 / 3 rem 4 band 5 rem 4 / 3 * 2"),
    tu:test_ast("1 + (2 - 3 | 4) ^ 5 | ((4 - 3 + 2) + 1 * 2) / 3 % 4 & 5 % 4 / 3 * 2",
        "1 + (2 - 3 bor 4) bxor 5 bor ((4 - 3 + 2) + 1 * 2) / 3 rem 4 band 5 rem 4 / 3 * 2"),
    ok.

block() ->
    tu:test_ast("begin { A }", "begin A end"),
    tu:test_ast("begin { A; B; }", "begin A, B end"),
    tu:test_ast("begin { A = 1; B = 2; A + B; }", "begin A = 1, B = 2, A + B end"),
    ok.

has() ->
    tu:test_ast("a in @B", "struct:has(B, a)"),
    tu:test_ast("\"a\" in @B", "struct:has(B, \"a\")"),
    tu:test_ast("\"a\" in @B.c", "struct:has(struct:get(B, 'B', c), \"a\")"),
    tu:test_ast("\"a\" in @B.c.d", "struct:has(struct:get(struct:get(B, 'B', c), c, d), \"a\")"),
    ok.

all() ->
    tu:test(?MODULE, send),
    tu:test(?MODULE, match),
    tu:test(?MODULE, bool),
    tu:test(?MODULE, comp),
    tu:test(?MODULE, concat),
    tu:test(?MODULE, add),
    tu:test(?MODULE, mul),
    tu:test(?MODULE, random),
    tu:test(?MODULE, block),
    tu:test(?MODULE, has),
    ok.
