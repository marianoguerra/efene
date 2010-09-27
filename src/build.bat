erlc fn_build.erl
erl -run fn_build build -run init stop -noshell
erlc fn.erl
erlc fn_lexpp.erl
erlc fn_shell.erl
erlc fn_pp.erl
erlc fn_errors.erl
erlc fn_gen.erl
erlc fn_meta.erl
move *.beam ../ebin
del fn_lexer.erl
del fn_parser.erl
