-module(fn_build).
-export([build/0]).

build() ->
    leex:file(fn_lexer),
    compile:file(fn_lexer),
    yecc:file(fn_parser, [{verbose, true}]),
    compile:file(fn_parser),
    ok.
