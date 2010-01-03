-module(fn_match).
-compile(export_all).

match([])                       -> [];
match([_|_] = List)             -> match_list(List);
match({integer, _, _}   = Ast)  -> Ast;
match({float, _, _}     = Ast)  -> Ast;
match({atom, _, _}      = Ast)  -> Ast;
match({string, _, _}    = Ast)  -> Ast;
match({var, _, _}       = Ast)  -> Ast;
match({nil, _}          = Ast)  -> Ast;
match(default)                  -> default;
match({cons, Line, A, B})       -> fn_gen:cons(Line, match(A), match(B));
match({tuple, Line, A})         -> fn_gen:tuple(Line, match_list(A));

match({'+' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'-' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'*' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'/' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'%', Line, A, B}) ->
    fn_gen:expr('rem', Line, match(A), match(B));

match({'++' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'--' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));

match({'<<', Line, A, B}) ->
    fn_gen:expr('bsl', Line, match(A), match(B));
match({'>>', Line, A, B}) ->
    fn_gen:expr('bsr', Line, match(A), match(B));

match({'<' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'<=', Line, A, B}) ->
    fn_gen:expr('=<', Line, match(A), match(B));
match({'==' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'===', Line, A, B}) ->
    fn_gen:expr('=:=', Line, match(A), match(B));
match({'>=' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'>' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));
match({'!=', Line, A, B}) ->
    fn_gen:expr('/=', Line, match(A), match(B));
match({'!==', Line, A, B}) ->
    fn_gen:expr('=/=', Line, match(A), match(B));

match({'!' = Op, Line, A, B}) ->
    fn_gen:expr(Op, Line, match(A), match(B));

match({'|', Line, A, B}) ->
    fn_gen:expr('bor', Line, match(A), match(B));
match({'&', Line, A, B}) ->
    fn_gen:expr('band', Line, match(A), match(B));
match({'^', Line, A, B}) ->
    fn_gen:expr('bxor', Line, match(A), match(B));

match({'and', Line, A, B}) ->
    fn_gen:expr('andalso', Line, match(A), match(B));
match({'or', Line, A, B}) ->
    fn_gen:expr('orelse', Line, match(A), match(B));

match({'not' = Op, Line, A}) ->
    fn_gen:expr(Op, Line, match(A));
match({'~', Line, A}) ->
    fn_gen:expr('bnot', Line, match(A));
match({'+', _Line, A}) ->
    match(A);
match({'-' = Op, Line, A}) ->
    fn_gen:expr(Op, Line, match(A));
match({'(', _Line, A}) ->
    match(A);

match({'bin'=Op, Line, BinElements}) ->
    fn_gen:op(Line, Op, match_list(BinElements));
match({'bin_element'=Op, Line, Value, Size, Types}) ->
    fn_gen:op(Line, Op, match(Value), match(Size), Types);

match({lc=Op, Line, Exp, Generators}) ->
    fn_gen:op(Line, Op, match(Exp), match_list(Generators));
match({generate=Op, Line, For, In}) ->
    fn_gen:op(Line, Op, match(For), match(In));
match({generate=Op, Line, For, In, If}) ->
    fn_gen:op(Line, Op, match(For), match(In), match(If));

match({bc=Op, Line, Exp, Generators}) -> {Op, Line, match(Exp), match_list(Generators)};
match({b_generate=Op, Line, For, In}) -> {Op, Line, match(For), match(In)};
match({b_generate=Op, Line, For, In, If}) -> {Op, Line, match(For), match(In), match(If)};
match({callatom, Line, [Atom], Args}) ->
    fn_gen:call(Line, Atom, match_list(Args));
match({callatom, Line, [Package, Function], Args}) ->
    fn_gen:call(Line, Package, Function, match_list(Args));
match({call, Line, A, Args}) ->
    fn_gen:call_expr(Line, match(A), match_list(Args));
match({'=', Line, A, B}) ->
    fn_gen:match(Line, match(A), match(B));
match({fn, Line, Patterns}) ->
    fn_gen:fun_def(Line, match_fun_body(Patterns));
match({fun_def, Line, Name, {fn, _Line, Patterns}}) ->
    fn_gen:func_def(Line, Name, get_function_arity(Patterns), match_function_body(Patterns));
match({obj_def, Line, Name, Fields}) ->
    fn_record:build(Line, Name, Fields);

match({'receive', Line, Patterns}) ->
    fn_gen:receive_expr(Line, match_function_body(Patterns));
match({'receive', Line, Patterns, After, {'{', _, AfterBody}}) ->
    fn_gen:receive_expr(Line, match_function_body(Patterns), match(After), match(AfterBody));

match({'try', Line, {'{', _, TryBody}, CatchPatterns}) ->
    fn_gen:try_expr(Line, match(TryBody), match_function_body(CatchPatterns));
match({'try', Line, {'{', _, TryBody}, CatchPatterns, {'{', _, FinallyBody}}) ->
    fn_gen:try_expr(Line, match(TryBody), match_function_body(CatchPatterns), match(FinallyBody));

match({'if', Line, Patterns}) ->
    fn_gen:if_expr(Line, match_function_body(Patterns));
match({'if', Line, Patterns, {'{', ElseLine, ElseBody}}) ->
    fn_gen:if_expr(Line, match_function_body(Patterns), ElseLine, match(ElseBody));

match({'case', Line, Condition, Patterns}) ->
    fn_gen:case_expr(Line, match(Condition), match_list(Patterns));
match({'case', Line, Condition, Patterns, {'{', ElseLine, ElseBody}}) ->
    fn_gen:case_expr(Line, match(Condition), match_list(Patterns), ElseLine, match(ElseBody));

match({clause, Line, Pattern, Guard, {'{', _BodyLine, Body}}) ->
    fn_gen:clause(Line, match_list(Pattern), match(Guard), match_list(Body));

match(Exp) -> {error, Exp}.

match_list([]) -> [];
match_list(Items) -> match_list(Items, []).

match_list([], Accum) -> lists:reverse(Accum);
match_list([Head | Tail], Accum) -> match_list(Tail, [match(Head) | Accum]).

match_fun_body(Patterns) ->
    fn_gen:clauses(match_function_body(Patterns)).

match_function_body(Patterns) -> match_function_body(Patterns, []).

match_function_body([], Clauses) -> lists:reverse(Clauses);
match_function_body([Pattern | Patterns], Clauses) ->
     match_function_body(Patterns, [match_pattern(Pattern) | Clauses]).

match_pattern({pattern, nil, {'(', Line, Guard}, {'{', _Line, Body}}) ->
     fn_gen:clause(Line, [], [match_list(Guard)], match_list(Body));
match_pattern({pattern, {'(', Line, Args}, [], {'{', _, Body}}) ->
     fn_gen:clause(Line, [match(Arg) || Arg <- Args], [], match_list(Body));
match_pattern({pattern, {'(', Line, Args}, Guards, {'{', _, Body}}) ->
     fn_gen:clause(Line, [match(Arg) || Arg <- Args], [match_list(Guards)], match_list(Body)).

get_function_arity([]) -> 0;
get_function_arity([{pattern, {'(', _Line, Arguments}, _, _}|_T]) -> length(Arguments).
