#!/usr/bin/env sh
echo "building efene compiler modules"
fnc build.fn
fn build build | grep -v "{yyaction"  | grep -v "{yeccpars" | grep -v "{yy_"
mv *.beam ../../bin
rm lexer.erl parser.erl
