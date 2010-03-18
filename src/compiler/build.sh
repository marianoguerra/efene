#!/usr/bin/env sh
echo "building efene compiler modules"
fnc build.fn
fn build build | grep -v "{yyaction"  | grep -v "{yeccpars" | grep -v "{yy_"
fnc fn_record.fn
fnc fn_match.fn
fnc fn_gen.fn
fnc fn_shell.fn
fnc fn_info.fn
fnc efene.fn
cp *.beam ../../bin
rm lexer.erl parser.erl
