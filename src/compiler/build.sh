#!/usr/bin/env sh
erlc build.erl
erl -run build build -run init stop -noshell | grep -v "{yeccpars" | grep -v "{yy_"
mv *.beam ../../bin
rm lexer.erl parser.erl
