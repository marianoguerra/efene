-module(typee).
-compile(export_all).

simple() ->
    tu:test_file("files/type/simple.ifn", "files/type/simple.erl"),
    ok.

all() ->
    tu:test(?MODULE, simple),
    ok.
