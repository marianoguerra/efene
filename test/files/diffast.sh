#!/usr/bin/env sh

fnc -t mod $1.ifn > fn_ast.out
fnc -t erl2ast $1.erl > erl_ast.out

diff fn_ast.out erl_ast.out
rm fn_ast.out erl_ast.out
