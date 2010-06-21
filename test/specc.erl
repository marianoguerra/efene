-module(specc).
-compile(export_all).

simple() ->
    tu:test_file("files/spec/one_type.ifn", "files/spec/one_type.erl"),
    tu:test_file("files/spec/unions.ifn", "files/spec/unions.erl"),
    ok.

all() ->
    tu:test(?MODULE, simple),
    ok.
