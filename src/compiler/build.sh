#!/usr/bin/env sh
echo "building the erlang modules"
erlc build.erl
erl -run build build -run init stop -noshell | grep -v "{yyaction"  | grep -v "{yeccpars" | grep -v "{yy_"
mv *.beam ../../bin
rm lexer.erl parser.erl
