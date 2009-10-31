-module(build).
-export([build/0]).

build() ->
    leex:file(lexer),
    yecc:file(parser),
    compile:file(lexer),
    compile:file(parser),
    compile:file(efene),
    ok.

