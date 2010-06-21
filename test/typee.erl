-module(typee).
-compile(export_all).

simple() ->
    tu:test_file("files/type/simple.ifn", "files/type/simple.erl"),
    ok.

params() ->
    tu:test_file("files/type/params.ifn", "files/type/params.erl"),
    ok.

all() ->
    tu:test(?MODULE, simple),
    tu:test(?MODULE, params),
    ok.
