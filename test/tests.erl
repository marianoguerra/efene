-module(tests).
-export([all/0]).

all() ->
    tu:test(literals),
    tu:test(unary),
    tu:test(exprs),
    ok.
