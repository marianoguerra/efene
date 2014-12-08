-module(tests).
-export([all/0]).

all() ->
    tu:test(literals),
    tu:test(unary),
    tu:test(exprs),
    tu:test(funs),
    tu:test(blocks),
    tu:test(comments),
    tu:test(module),
    tu:test(records),
    tu:test(attrs),
    tu:test(specc),
    tu:test(typee),
    tu:test(endlines),
    tu:test(timport),
    ok.
