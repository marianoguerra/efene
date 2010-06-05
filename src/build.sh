#!/usr/bin/env sh
erlc fn_build.erl
erl -run fn_build build -run init stop -noshell | grep -v "{yyaction"  | grep -v "{yeccpars" | grep -v "{yy_"
erlc fn.erl
erlc fn_lexpp.erl
erlc fn_shell.erl
erlc fn_pp.erl
erlc fn_errors.erl
erlc fn_gen.erl
erlc fn_object.erl
erlc fn_server.erl
mv *.beam ../ebin
rm fn_lexer.erl
rm fn_parser.erl
cd ../lib
./build.sh
cd ../src

