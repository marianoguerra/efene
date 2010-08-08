-module(fn_build).
-export([build/0]).

-license("New BSD License, part of efene, see LICENSE for details").

build() ->
    leex:file(fn_lexer),
    compile:file(fn_lexer),
    yecc:file(fn_parser),
    compile:file(fn_parser),
    ok.
