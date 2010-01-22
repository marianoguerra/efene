-module(build).
-export([build/0]).

build() ->
    leex:file(lexer),
    yecc:file(parser),
    compile:file(lexer),
    compile:file(parser),
    compile:file(fn_record),
    compile:file(fn_match),
    compile:file(fn_prettypr),
    compile:file(fn_gen),
    compile:file(efene),
    ok.

