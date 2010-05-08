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
    ok.
