-module(specc).
-compile(export_all).

simple() ->
    tu:test_file("files/spec/one_type.ifn", "files/spec/one_type.erl"),
    ok.

all() ->
    tu:test(?MODULE, simple),
    ok.
