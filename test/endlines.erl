-module(endlines).
-compile(export_all).

simple() ->
    tu:test_file_fn("files/endlines/testnix.ifn", "files/endlines/testwin.ifn"),
    ok.

all() ->
    tu:test(?MODULE, simple),
    ok.
