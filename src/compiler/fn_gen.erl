-module(fn_gen).
-compile(export_all).

call(Line, Function, Args) ->
    {call, Line, Function, Args}.

call(Line, Package, Function, Args) ->
    {call, Line, {remote, Line, Package, Function}, Args}.

call_expr(Line, Expr, Args) ->
    {call, Line, Expr, Args}.

func_def(Line, Name, Arity, Ast) ->
    {function, Line, Name, Arity, Ast}.

fun_def(Line, Patterns) ->
    {'fun', Line, Patterns}.

try_expr(Line, Body, Patterns) ->
    {'try', Line, Body, [], Patterns, []}.

try_expr(Line, Body, Patterns, Finally) ->
    {'try', Line, Body, [], Patterns, Finally}.

if_expr(Line, Patterns) ->
    {'if', Line, Patterns}.

if_expr(Line, Patterns, ElseLine, ElseBody) ->
    {'if', Line, Patterns ++ [clause(Line, [], [[{atom, ElseLine, true}]], ElseBody)]}.

case_expr(Line, Condition, Patterns) ->
    {'case', Line, Condition, Patterns}.

case_expr(Line, Condition, Patterns, ElseLine, ElseBody) ->
    {'case', Line, Condition, Patterns ++ [clause(Line, [{var, ElseLine, '_'}], [], ElseBody)]}.

clause(Line, Pattern, Guard, Body) ->
    {clause, Line, Pattern, Guard, Body}.

receive_expr(Line, Patterns) ->
    {'receive', Line, Patterns}.

receive_expr(Line, Patterns, After, AfterBody) ->
    {'receive', Line, Patterns, After, AfterBody}.

module(Name, Ast) ->
    [{attribute, 1, module, Name}, {attribute, 1, compile, export_all}] ++ Ast.

match(Line, Ast1, Ast2) -> {match, Line, Ast1, Ast2}.

expr(Op, Line, Ast1) -> {op, Line, Op, Ast1}.
expr(Op, Line, Ast1, Ast2) -> {op, Line, Op, Ast1, Ast2}.

clauses(Patterns) ->
    {clauses, Patterns}.

tuple(Line, Items) ->
    {tuple, Line, Items}.

cons(Line, Head, Tail) ->
    {cons, Line, Head, Tail}.

op(Line, Op, First) ->
    {Op, Line, First}.

op(Line, Op, First, Second) ->
    {Op, Line, First, Second}.

op(Line, Op, First, Second, Third) ->
    {Op, Line, First, Second, Third}.
