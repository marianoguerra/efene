-module(typee).
-compile(export_all).

simple() ->
    tu:test_file("files/type/simple.ifn", "files/type/simple.erl"),
    ok.

params() ->
    tu:test_file("files/type/tparams.ifn", "files/type/tparams.erl"),
    ok.

all_erlang_types() ->
    tu:test_file("files/type/all_types.ifn", "files/type/all_types.erl"),
    ok.

all() ->
    tu:test(?MODULE, simple),
    tu:test(?MODULE, params),
    tu:test(?MODULE, all_erlang_types),
    ok.
