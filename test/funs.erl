-module(funs).
-compile(export_all).

basic() ->
    tu:test_ast("fn () { ok }", "fun () -> ok end"),
    tu:test_ast("fn () { 1 + 2 * 3 - 4 / 5 }", "fun () -> 1 + 2 * 3 - 4 / 5 end"),
    tu:test_ast("fn (A) { A }", "fun (A) -> A end"),
    tu:test_ast("fn (A, B, C) { A + B + C }", "fun (A, B, C) -> A + B + C end"),
    ok.

multiple() ->
    tu:test_ast("fn (1) { 2 } fn (2) { 3 }", "fun (1) -> 2; (2) -> 3 end"),
    tu:test_ast("fn (1) { 2 } fn (2) { 3 } fn (A) { A + 1 }",
        "fun (1) -> 2; (2) -> 3; (A) -> A + 1 end"),
    ok.

guards() ->
    tu:test_ast("fn (A) when A > 0 { A }", "fun (A) when A > 0 -> A end"),
    tu:test_ast("fn (A) when A > 0 and A != 2 { A }",
        "fun (A) when A > 0 andalso A /= 2 -> A end"),
    tu:test_ast("fn (A) when (A > 0) andd (A != 2) { A }",
        "fun (A) when (A > 0) and (A /= 2) -> A end"),
    tu:test_ast("fn (A) when A > 0 or A != 2 { A }",
        "fun (A) when A > 0 orelse A /= 2 -> A end"),
    tu:test_ast("fn (A) when (A > 0) orr (A != 2) { A }",
        "fun (A) when (A > 0) or (A /= 2) -> A end"),
    ok.

patternmatch() ->
    tu:test_ast("fn (a, B, C) { B + C }", "fun (a, B, C) -> B + C end"),
    tu:test_ast("fn ((a, B), C) { B + C }", "fun ({a, B}, C) -> B + C end"),
    tu:test_ast("fn ((a, B)=A, _C) { A }", "fun ({a, B}=A, _C) -> A end"),
    tu:test_ast("fn ([H:T]) { (H, T) }", "fun ([H|T]) -> {H, T} end"),
    tu:test_ast("fn ([H1, H2:T]) { (H1, H2, T) }", "fun ([H1, H2|T]) -> {H1, H2, T} end"),
    tu:test_ast("fn ([H1, H2:T]) { (H1, H2, T) }",
        "fun ([H1, H2|T]) -> {H1, H2, T} end"),
    tu:test_ast("fn (person.P[name=\"foo\"]=A) { A }",
        "fun (P#person{name=\"foo\"}=A) -> A end"),
    ok.

all() ->
    tu:test(?MODULE, basic),
    tu:test(?MODULE, multiple),
    tu:test(?MODULE, guards),
    tu:test(?MODULE, patternmatch),
    ok.
