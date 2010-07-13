-module(fn_meta).
-export([eval/1, astify/2]).

eval(Ast) ->
    Bindings = erl_eval:new_bindings(),
    {value, Result, _NewBindings} = erl_eval:expr(Ast, Bindings),
    Result.

astify(Line, Val) ->
    fn_gen:literal_to_ast(Val, Line).
