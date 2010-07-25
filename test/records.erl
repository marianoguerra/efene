-module(records).
-compile(export_all).

complex() ->
    tu:test_file("files/record/test.ifn", "files/record/test.erl"),
    ok.

all() ->
    tu:test(?MODULE, complex).

