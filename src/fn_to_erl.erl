-module(fn_to_erl).
-export([ast_to_ast/1]).

% sequence
-define(S(Line, Type, Val), {seq, Line, Type, Val}).
% val
-define(V(Line, Type, Val), {val, Line, Type, Val}).
% expression
-define(E(Line, Type, Val), {expr, Line, Type, Val}).
% operation
-define(O(Line, Type, Left, Right), {op, Line, Type, Left, Right}).
% unary operation
-define(UO(Line, Type, Val), {unary_op, Line, Type, Val}).
% tag
-define(T(Line, Tag, Val), {tag, Line, Tag, Val}).

-define(Atom(Val), ?V(_, atom, Val)).

ast_to_ast(Nodes) when is_list(Nodes) -> ast_to_ast(Nodes, []);

ast_to_ast({attr, Line, export, ?S(_PLine, list, Params), noresult}) ->
    {attribute, Line, export, lists:map(fun ast_to_export_fun/1, Params)};

ast_to_ast(?E(Line, call_do, {Place, Call, Fun})) ->
    {call, Line, FCall, Args} = ast_to_ast(Call),
    EFun = ast_to_ast(Fun),
    AllArgs = case Place of
                  first -> [EFun|Args];
                  last  -> Args ++ [EFun]
              end,
    {call, Line, FCall, AllArgs};

ast_to_ast(?E(_Line, call_thread, {InitialVal, Calls})) ->
    Threaded = lists:foldl(fun (Current, Accum) ->
                                   {Pos, Call} = Current,
                                   ?E(CallLine, call, {Fun, Args}) = Call,
                                   NewArgs = case Pos of
                                                 first -> [Accum|Args];
                                                 last -> Args ++ [Accum]
                                             end,
                                   ?E(CallLine, call, {Fun, NewArgs})
                end, InitialVal, Calls),
    ast_to_ast(Threaded);

ast_to_ast(?S(Line, list, Val)) -> list_to_cons_list(Line, Val);

ast_to_ast(?S(Line, map=Type, {Var, KVs})) ->
    {Type, Line, ast_to_ast(Var), lists:map(fun to_map_field/1, KVs)};
ast_to_ast(?S(Line, map=Type, KVs)) ->
    {Type, Line, lists:map(fun to_map_field/1, KVs)};

ast_to_ast(?T(Line, [?Atom(r), ?Atom(RecordName)],
              ?S(_MapLine, map, {Var, KVs}))) ->
    {record, Line, ast_to_ast(Var), RecordName, lists:map(fun to_record_field/1, KVs)};
ast_to_ast(?T(Line, [?Atom(r), ?Atom(RecordName)], ?S(_MapLine, map, KVs))) ->
    {record, Line, RecordName, lists:map(fun to_record_field/1, KVs)};

ast_to_ast(?T(Line, [?Atom(c)], ?V(_StrLine, string, [Char]))) ->
    {char, Line, Char};
ast_to_ast(?T(Line, [?Atom(atom)], ?V(_StrLine, string, AtomStr))) ->
    {atom, Line, list_to_atom(AtomStr)};
ast_to_ast(?S(Line, tuple=Type, Val))   -> {Type, Line, ast_to_ast(Val)};
ast_to_ast(?S(Line, cons=Type, {H, T})) ->
    {Type, Line, ast_to_ast(H), ast_to_ast(T)};

ast_to_ast(?V(Line, fn_ref, {{Mod, Fun}, Arity})) ->
    {'fun', Line, {function, unwrap(Mod), unwrap(Fun), unwrap(Arity)}};

ast_to_ast(?V(Line, fn_ref, {Fun, Arity})) ->
    {'fun', Line, {function, unwrap(Fun), unwrap(Arity)}};

ast_to_ast(?E(Line, 'when', Clauses)) ->
    {'if', Line, ast_to_ast(Clauses)};

ast_to_ast({wcond, Line, Cond, Body}) ->
    {clause, Line, [], ast_to_ast(Cond), ast_to_ast(Body)};

ast_to_ast({welse, Line, Body}) ->
    {clause, Line, [], [{atom, Line, true}], ast_to_ast(Body)};

ast_to_ast(?E(Line, 'for', {Qualifiers, Body})) ->
    EBody = case Body of
                [Node] -> ast_to_ast(Node);
                Nodes -> {block, Line, ast_to_ast(Nodes)}
            end,
    {lc, Line, EBody, lists:map(fun for_qualifier_to_ast/1, Qualifiers)};

% TODO: fix case: to match some default value or reject it
% todo, restrict tuple to 1 or two items, the first being throw, error or exit
ast_to_ast(?E(Line, 'try', {Body, Catch, After})) ->
    EBody = ast_to_ast(Body), 
    ECatch = case Catch of
                 ?E(_CLine, 'case', Clauses) -> lists:map(fun ast_to_catch/1, Clauses);
                 nocatch -> []
             end,

    EAfter = case After of
                 noafter -> [];
                 AfterBody -> ast_to_ast(AfterBody)
             end,
    {'try', Line, EBody, [], ECatch, EAfter};

% TODO: fix case: to match some default value or reject it
ast_to_ast(?E(Line, 'receive', {?E(_CLine, 'case', Clauses), noafter})) ->
    EClauses = ast_to_ast(Clauses),
    {'receive', Line, EClauses};

ast_to_ast(?E(Line, 'receive', {?E(_CLine, 'case', Clauses), {After, AfterBody}})) ->
    EClauses = ast_to_ast(Clauses),
    {'receive', Line, EClauses, ast_to_ast(After), ast_to_ast(AfterBody)};

% TODO: fix case: to match some default value or reject it
ast_to_ast(?E(Line, switch, {Value, ?E(_CaseLine, 'case', Clauses)})) ->
    EClauses = ast_to_ast(Clauses),
    TupleClauses = lists:map(fun to_tuple_clause/1, EClauses),
    {'case', Line, ast_to_ast(Value), TupleClauses};

ast_to_ast({cmatch, Line, {Conds, When, Body}}) ->
    EConds = ast_to_ast(Conds),
    {clause, Line, EConds, when_to_ast(When), ast_to_ast(Body)};

% TODO: match the number of arguments when case is in a fun call
ast_to_ast({celse, Line, Body}) ->
    {clause, Line, [{var, Line, '_'}], [], ast_to_ast(Body)};

ast_to_ast(?E(Line, 'begin', Body)) ->
    {block, Line, ast_to_ast(Body)};

% TODO: attrs
% TODO: check that arities on match are equal and that there's only one
% case if using empty case (arity 0 fun)
ast_to_ast(?E(Line, fn, {Name, _Attrs, ?E(_CLine, 'case', Cases)})) ->
    [FirstCase|_TCases] = Cases,
    {cmatch, _FCLine, {FCCond, _FCWhen, _FCBody}} = FirstCase,
    Arity = length(FCCond),
    {ok, FixedCases} = expand_case_else_match(Cases),
    {function, Line, unwrap(Name), Arity, ast_to_ast(FixedCases)};

ast_to_ast(?E(Line, fn, ?E(_CLine, 'case', Cases))) ->
    {ok, FixedCases} = expand_case_else_match(Cases),
    {'fun', Line, {clauses, ast_to_ast(FixedCases)}};

ast_to_ast(?E(Line, call, {{Mod, Fun}, Args})) ->
    EMod = ast_to_ast(Mod),
    EFun = ast_to_ast(Fun),
    EArgs = ast_to_ast(Args),
    {call, Line, {remote, Line, EMod, EFun}, EArgs};

ast_to_ast(?E(Line, call, {Fun, Args})) ->
    EFun = ast_to_ast(Fun),
    EArgs = ast_to_ast(Args),
    {call, Line, EFun, EArgs};

ast_to_ast(?O(Line, Op, Left, Right)) ->
    {op, Line, map_op(Op), ast_to_ast(Left), ast_to_ast(Right)};

ast_to_ast(?V(Line, atom=Type, Val))    -> {Type, Line, Val};
ast_to_ast(?V(Line, integer=Type, Val)) -> {Type, Line, Val};
ast_to_ast(?V(Line, float=Type, Val))   -> {Type, Line, Val};
ast_to_ast(?V(Line, boolean, Val))      -> {atom, Line, Val};
ast_to_ast(?V(Line, var=Type, Val))     -> {Type, Line, Val};
ast_to_ast(?V(Line, string=Type, Val))  -> {Type, Line, Val};
ast_to_ast(?V(Line, bstring, Val)) ->
    {bin, Line, [{bin_element, 5, {string, Line, Val}, default, default}]};

ast_to_ast(?UO(Line, Op, Val)) ->
    {op, Line, map_op(Op), ast_to_ast(Val)}.

% {clause, Line, Match, When, Body}

ast_to_ast([], Accum) ->
    lists:reverse(Accum);
ast_to_ast([H|T], Accum) ->
    ast_to_ast(T, [ast_to_ast(H)|Accum]).

map_op('+') -> '+';
map_op('-') -> '-';
map_op('*') -> '*';
map_op('/') -> '/';
map_op('//') -> 'div';
map_op('%') -> 'rem';
map_op('|') -> 'bor';
map_op('&') -> 'band';
map_op('^') -> 'bxor';
map_op('>>') -> 'bsr';
map_op('<<') -> 'bsl';
map_op('~') -> 'bnot';
map_op('and') -> 'andalso';
map_op('andd') -> 'and';
map_op('or') -> 'orelse';
map_op('orr') -> 'or';
map_op('xor') -> 'xor';
map_op('!') -> '!';
map_op('not') -> 'not';
map_op('++') -> '++';
map_op('--') -> '--';
map_op('<') -> '<';
map_op('<=') -> '=<';
map_op('>') -> '>';
map_op('>=') -> '>=';
map_op('==') -> '==';
map_op('===') -> '=:=';
map_op('!=') -> '/=';
map_op('!==') -> '=/=';
map_op('=') -> '='.

list_to_cons_list(Line, Val) ->
    list_to_cons_list_r(Line, lists:reverse(Val), {nil, Line}).

list_to_cons_list_r(_Line, [], Cons) ->
    Cons;

list_to_cons_list_r(Line, [H|T], Cons) ->
    list_to_cons_list_r(Line, T, {cons, Line, ast_to_ast(H), Cons}).

ast_to_export_fun(?O(_Line, '/', ?V(_ALine, atom, FunName), ?V(_ArLine, integer, Arity))) ->
    {FunName, Arity}.

ast_to_catch({cmatch, Line, {[Match], When, Body}}) ->
    EMatch = {tuple, Line, [{atom, Line, throw}, ast_to_ast(Match), {var, Line, '_'}]},
    EBody = ast_to_ast(Body),
    EWhen = when_to_ast(When),
    {clause, Line, [EMatch], EWhen, EBody};
% TODO: check that ClassName is throw, error or exit
ast_to_catch({cmatch, Line, {[?V(_ALine, atom, ClassName), Match], When, Body}}) ->
    EMatch = {tuple, Line, [{atom, Line, ClassName}, ast_to_ast(Match), {var, Line, '_'}]},
    EBody = ast_to_ast(Body),
    EWhen = when_to_ast(When),
    {clause, Line, [EMatch], EWhen, EBody};
ast_to_catch({celse, Line, Body}) ->
    EMatch = {tuple, Line, [{var, Line, '_'}, {var, Line, '_'}, {var, Line, '_'}]},
    EBody = ast_to_ast(Body),
    {clause, Line, [EMatch], [], EBody}.
% TODO: catch the rest and accumulate the error

when_to_ast(nowhen) -> [];
when_to_ast(When) when is_list(When) ->
    lists:map(fun when_to_ast/1, When);
when_to_ast(When) ->
    ast_to_ast(When).

to_map_field({kv, Line, Key, Val}) ->
    {map_field_assoc, Line, ast_to_ast(Key), ast_to_ast(Val)};
to_map_field({kvmatch, Line, Key, Val}) ->
    {map_field_exact, Line, ast_to_ast(Key), ast_to_ast(Val)}.

to_record_field({kv, Line, Key, Val}) ->
    {record_field, Line, ast_to_ast(Key), ast_to_ast(Val)}.

% erlang ast
% XXX for now empty case in switch matches the empty tuple
to_tuple_clause({clause, Line, [], Guard, Body}) ->
    {clause, Line, [{tuple, Line, []}], Guard, Body};
to_tuple_clause({clause, _Line, [_Match], _Guard, _Body}=Ast) ->
    Ast;
to_tuple_clause({clause, Line, Matches, Guard, Body}) ->
    {clause, Line, [{tuple, Line, Matches}], Guard, Body}.

for_qualifier_to_ast({filter, Ast}) -> ast_to_ast(Ast);
for_qualifier_to_ast({generate, Line, Left, Right}) ->
    {generate, Line, ast_to_ast(Left), ast_to_ast(Right)}.

expand_case_else_match([{cmatch, _Line, {Matches, _When, _Body}}=H|T]) ->
    Arity = length(Matches),
    expand_case_else_match(T, Arity, [H]).

expand_case_else_match([], _Arity, Accum) ->
    {ok, lists:reverse(Accum)};
expand_case_else_match([{celse, Line, Body}|T], Arity, Accum) ->
    Matches = [?V(Line, var, '_') || _ <- lists:seq(1, Arity)],
    NewElse = {cmatch, Line, {Matches, nowhen, Body}},
    expand_case_else_match(T, Arity, [NewElse|Accum]);
expand_case_else_match([H|T], Arity, Accum) ->
    expand_case_else_match(T, Arity, [H|Accum]).

unwrap(?V(_Line, _Type, Val)) -> Val.
