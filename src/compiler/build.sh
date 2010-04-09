#!/usr/bin/env sh
echo "building efene compiler modules"
fnc build.fn
fn build build | grep -v "{yyaction"  | grep -v "{yeccpars" | grep -v "{yy_"
fnc fn_record.fn fn_match.fn fn_gen.fn fn_shell.fn fn_info.fn efene.fn
mv *.beam ../../bin
rm lexer.erl parser.erl
