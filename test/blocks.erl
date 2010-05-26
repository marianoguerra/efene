-module(blocks).
-compile(export_all).

ifs() ->
    tu:test_ast("when A { B }", "if A -> B end"),
    tu:test_ast("when 1 { 1 } else when 2 { 2 }", "if 1 -> 1; 2 -> 2 end"),
    tu:test_ast("when 1 { 1 } else when 2 { 2 } else when 3 { 3 }",
        "if 1 -> 1; 2 -> 2; 3 -> 3 end"),
    tu:test_ast("when 1 { 1 } else when 2 { 2 } else when 3 { 3 } else { 4 }",
        "if 1 -> 1; 2 -> 2; 3 -> 3; true -> 4 end"),
    ok.

tries() ->
    tu:test_ast("try { A } catch B { C }", "try A catch B -> C end"),
    tu:test_ast("try { A } catch 1 { 1 } catch 2 { 2 }",
        "try A catch 1 -> 1; 2 -> 2 end"),
    tu:test_ast("try { A } catch 1 { 1 } catch 2 { 2 } catch 3 { 3 }",
        "try A catch 1 -> 1; 2 -> 2; 3 -> 3 end"),
    tu:test_ast("try { A } catch error E { E } catch throw T { T }",
        "try A catch error : E -> E; throw:T -> T end"),
    tu:test_ast("try { A } catch exit E { E } catch Type T { (Type, T) }",
        "try A catch exit:E -> E; Type:T -> {Type, T} end"),
    tu:test_ast("try { A } catch B { C } after { true }",
        "try A catch B -> C after true end"),
    ok.

cases() ->
    tu:test_ast("switch A { case 1 { 1 } case 2 { 2 }; }",
        "case A of 1 -> 1; 2 -> 2 end"),
    tu:test_ast("switch A { case 1 { 1 } case 2 { 2 } else { 3 }; }",
        "case A of 1 -> 1; 2 -> 2; _ -> 3 end"),
    ok.

receives() ->
    tu:test_ast("receive 1 { 1 }", "receive 1 -> 1 end"),
    tu:test_ast("receive 1 { 1 } after 100 { 2 }",
        "receive 1 -> 1 after 100 -> 2end"),
    tu:test_ast("receive 1 { 1 } else receive 2 { 2 }",
        "receive 1 -> 1; 2 -> 2 end"),
    ok.

all() ->
    tu:test(?MODULE, ifs),
    tu:test(?MODULE, tries),
    tu:test(?MODULE, cases),
    tu:test(?MODULE, receives),
    ok.
