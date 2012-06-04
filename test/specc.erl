-module(specc).
-compile(export_all).

simple() ->
    tu:test_file("files/spec/one_type.ifn", "files/spec/one_type.erl"),
    ok.

unions() ->
    tu:test_file("files/spec/unions.ifn", "files/spec/unions.erl"),
    ok.

params() ->
    tu:test_file("files/spec/sparams.ifn", "files/spec/sparams.erl"),
    ok.

annotation() ->
    tu:test_file("files/spec/coloncolon.ifn", "files/spec/coloncolon.erl"),
    ok.

range() ->
    tu:test_file("files/spec/range.ifn", "files/spec/range.erl"),
    ok.

all() ->
    tu:test(?MODULE, simple),
    tu:test(?MODULE, unions),
    tu:test(?MODULE, params),
    tu:test(?MODULE, annotation),
    tu:test(?MODULE, range),
    ok.
