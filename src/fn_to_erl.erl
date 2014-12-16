%% Copyright 2015 Mariano Guerra
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(fn_to_erl).
-export([ast_to_ast/2, to_erl/2]).

-include("efene.hrl").

new_state(Module) -> #{module => Module, errors => [], warnings => [], attrs => [], level => 0}.

to_erl(Ast, Module) -> ast_to_ast(Ast, new_state(Module)).

ast_to_ast(Nodes, State) when is_list(Nodes) -> ast_to_ast(Nodes, [], State);

ast_to_ast({attr, Line, [?V(_ELine, atom, export)], Params, noresult}, #{level := 0}=State) ->
    {EFuns, State1} = state_map(fun ast_to_export_fun/2, Params, State),
    R = {attribute, Line, export, EFuns},
    {R, State1};

ast_to_ast(?E(Line, fn, {Name, Attrs, ?E(_CLine, 'case', Cases)}), #{level := 0}=State) ->
    [FirstCase|_TCases] = Cases,
    {cmatch, _FCLine, {FCCond, _FCWhen, _FCBody}} = FirstCase,
    Arity = length(FCCond),
    {ok, FixedCases} = expand_case_else_match(Cases),
    StateLevel1 = State#{level => 1},
    {EFixedCases, State1} = ast_to_ast(FixedCases, StateLevel1),
    BareName = unwrap(Name),
    State2 = add_attributes(State1, fn, Line, {BareName, Arity}, Attrs),
    R = {function, Line, BareName, Arity, EFixedCases},
    State3 = check_case_arities_equal(Cases, State2, Arity),
    {R, State3#{level => 0}};

ast_to_ast({attr, Line, [?Atom(record)], [?Atom(RecordName)], ?S(_TLine, tuple, Fields)},
           #{level := 0}=State) ->
    {RFields, State1} = lists:mapfoldl(fun to_record_field_decl/2, State#{level => 1}, Fields),
    R = {attribute, Line, record, {RecordName, RFields}},
    {R, State1#{level => 0}};

ast_to_ast(Ast, #{level := 0}=State) ->
    Line = element(2, Ast),
    State1 = add_error(State, invalid_top_level_expression, Line, {ast, Ast}),
    R = {atom, Line, error},
    {R, State1};

ast_to_ast(?E(Line, call_do, {Place, Call, Fun}), State) ->
    {{call, Line, FCall, Args}, State1} = ast_to_ast(Call, State),
    {EFun, State2} = ast_to_ast(Fun, State1),
    AllArgs = case Place of
                  first -> [EFun|Args];
                  last  -> Args ++ [EFun]
              end,
    {{call, Line, FCall, AllArgs}, State2};

ast_to_ast(?E(_Line, call_thread, {InitialVal, Calls}), State) ->
    Threaded = lists:foldl(fun (Current, Accum) ->
                                   {Pos, Call} = Current,
                                   ?E(CallLine, call, {Fun, Args}) = Call,
                                   NewArgs = case Pos of
                                                 first -> [Accum|Args];
                                                 last -> Args ++ [Accum]
                                             end,
                                   ?E(CallLine, call, {Fun, NewArgs})
                end, InitialVal, Calls),
    ast_to_ast(Threaded, State);

ast_to_ast(?T(Line, [?Atom(b)], ?S(_LLine, list, TSList)), State) ->
    type_specifiers_to_ast(Line, TSList, State);

ast_to_ast(?S(Line, list, Val), State) ->
    list_to_cons_list(Line, Val, State);

ast_to_ast(?S(Line, map=Type, {Var, KVs}), State) ->
    {EVar, State1} = ast_to_ast(Var, State),
    {Items, State2} = state_map(fun to_map_field/2, KVs, State1),
    R = {Type, Line, EVar, Items},
    {R, State2};
ast_to_ast(?S(Line, map=Type, KVs), State) ->
    {Items, State1} = state_map(fun to_map_field/2, KVs, State),
    R = {Type, Line, Items},
    {R, State1};

ast_to_ast(?T(Line, [?Atom(i)], ?V(_VLine, string, Str)), State) ->
    info_to_ast(Line, Str, State);
ast_to_ast(?T(Line, [?Atom(r), ?Atom(RecordName)],
              ?S(_MapLine, map, {Var, KVs})), State) ->
    {EVar, State1} = ast_to_ast(Var, State),
    {Items, State2} = lists:map(fun to_record_field/2, KVs, State1),
    R = {record, Line, EVar, RecordName, Items},
    {R, State2};
ast_to_ast(?T(Line, [?Atom(r), ?Atom(RecordName)], ?S(_MapLine, map, KVs)), State) ->
    {Items, State1} = state_map(fun to_record_field/2, KVs, State),
    R = {record, Line, RecordName, Items},
    {R, State1};

ast_to_ast(?T(Line, [?Atom(c)], ?V(_StrLine, string, [Char])), State) ->
    {{char, Line, Char}, State};
ast_to_ast(?T(Line, [?Atom(atom)], ?V(_StrLine, string, AtomStr)), State) ->
    {{atom, Line, list_to_atom(AtomStr)}, State};
ast_to_ast(?S(Line, tuple=Type, Val), State)   ->
    {EVal, State1} = ast_to_ast(Val, State),
    {{Type, Line, EVal}, State1};
ast_to_ast(?S(Line, cons=Type, {H, T}), State) ->
    {EH, State1} = ast_to_ast(H, State),
    {ET, State2} = ast_to_ast(T, State1),
    R = {Type, Line, EH, ET},
    {R, State2};

ast_to_ast(?V(Line, fn_ref, {{Mod, Fun}, Arity}), State) ->
    R = {'fun', Line, {function, unwrap(Mod), unwrap(Fun), unwrap(Arity)}},
    {R, State};

ast_to_ast(?V(Line, fn_ref, {Fun, Arity}), State) ->
    R = {'fun', Line, {function, unwrap(Fun), unwrap(Arity)}},
    {R, State};

ast_to_ast(?E(Line, 'when', Clauses), State) ->
    {EClauses, State1} = ast_to_ast(Clauses, State),
    R = {'if', Line, EClauses},
    {R, State1};

ast_to_ast({wcond, Line, Cond, Body}, State) ->
    {ECond, State1} = ast_to_ast(Cond, State),
    {EBody, State2} = ast_to_ast(Body, State1),
    R = {clause, Line, [], ECond, EBody},
    {R, State2};

ast_to_ast({welse, Line, Body}, State) ->
    {EBody, State1} = ast_to_ast(Body, State),
    R = {clause, Line, [], [{atom, Line, true}], EBody},
    {R, State1};

ast_to_ast(?T(_TLine, [?Atom(b)], ?E(Line, 'for', {Qualifiers, Body})), State) ->
    {Items, EBody, State1} = lc_to_ast(Line, Qualifiers, Body, State),
    R = {bc, Line, EBody, Items},
    {R, State1};
ast_to_ast(?E(Line, 'for', {Qualifiers, Body}), State) ->
    {Items, EBody, State1} = lc_to_ast(Line, Qualifiers, Body, State),
    R = {lc, Line, EBody, Items},
    {R, State1};

ast_to_ast(?E(Line, 'try', {Body, Catch, After}), State) ->
    {EBody, State1} = ast_to_ast(Body, State),
    {ECatch, State2} = case Catch of
                           ?E(_CLine, 'case', Clauses) ->
                               state_map(fun ast_to_catch/2, Clauses, State);
                           nocatch -> {[], State1}
                       end,

    {EAfter, State3} = case After of
                           noafter -> {[], State2};
                           AfterBody -> ast_to_ast(AfterBody, State2)
                       end,
    R = {'try', Line, EBody, [], ECatch, EAfter},
    {R, State3};

ast_to_ast(?E(Line, 'receive', {?E(_CLine, 'case', Clauses), noafter}), State) ->
    {EClauses, State1} = ast_to_ast(Clauses, State),
    TupleClauses = lists:map(fun to_tuple_clause/1, EClauses),
    R= {'receive', Line, TupleClauses},
    {R, State1};

ast_to_ast(?E(Line, 'receive', {?E(_CLine, 'case', Clauses), {After, AfterBody}}), State) ->
    {EClauses, State1} = ast_to_ast(Clauses, State),
    TupleClauses = lists:map(fun to_tuple_clause/1, EClauses),
    {EAfter, State2} = ast_to_ast(After, State1),
    {EAfterBody, State3} = ast_to_ast(AfterBody, State2),
    R = {'receive', Line, TupleClauses, EAfter, EAfterBody},
    {R, State3};

ast_to_ast(?E(Line, switch, {Value, ?E(_CaseLine, 'case', Clauses)}), State) ->
    {EClauses, State1} = ast_to_ast(Clauses, State),
    TupleClauses = lists:map(fun to_tuple_clause/1, EClauses),
    {EValue, State2} = ast_to_ast(Value, State1),
    R = {'case', Line, EValue, TupleClauses},
    {R, State2};

ast_to_ast({cmatch, Line, {Conds, When, Body}}, State) ->
    {EConds, State1} = ast_to_ast(Conds, State),
    {EWhen, State2} = when_to_ast(When, State1),
    {EBody, State3} = ast_to_ast(Body, State2),
    R = {clause, Line, EConds, EWhen, EBody},
    {R, State3};

ast_to_ast({celse, Line, Body}, State) ->
    {EBody, State1} = ast_to_ast(Body, State),
    R = {clause, Line, [{var, Line, '_'}], [], EBody},
    {R, State1};

ast_to_ast(?E(Line, 'begin', Body), State) ->
    {EBody, State1} = ast_to_ast(Body, State),
    R = {block, Line, EBody},
    {R, State1};

ast_to_ast(?E(Line, fn, ?E(_CLine, 'case', Cases)), State) ->
    {ok, FixedCases} = expand_case_else_match(Cases),
    {EFixedCases, State1} = ast_to_ast(FixedCases, State),
    R = {'fun', Line, {clauses, EFixedCases}},
    {R, State1};

ast_to_ast(?E(Line, fn, {?V(_VLine, var, FName), ?E(_CLine, 'case', Cases)}), State) ->
    {ok, FixedCases} = expand_case_else_match(Cases),
    {EFixedCases, State1} = ast_to_ast(FixedCases, State),
    R = {named_fun, Line, FName, EFixedCases},
    {R, State1};

ast_to_ast(?E(Line, call, {{Mod, Fun}, Args}), State) ->
    {EMod, State1} = ast_to_ast(Mod, State),
    {EFun, State2} = ast_to_ast(Fun, State1),
    {EArgs, State3} = ast_to_ast(Args, State2),
    R = {call, Line, {remote, Line, EMod, EFun}, EArgs},
    {R, State3};

ast_to_ast(?E(Line, call, {Fun, Args}), State) ->
    {EFun, State1} = ast_to_ast(Fun, State),
    {EArgs, State2} = ast_to_ast(Args, State1),
    R = {call, Line, EFun, EArgs},
    {R, State2};

ast_to_ast(?O(Line, Op, Left, Right), State) ->
    {ELeft, State1} = ast_to_ast(Left, State),
    {ERight, State2} = ast_to_ast(Right, State1),
    R = {op, Line, map_op(Op), ELeft, ERight},
    {R, State2};

ast_to_ast(?V(Line, atom=Type, Val), State)    -> {{Type, Line, Val}, State};
ast_to_ast(?V(Line, integer=Type, Val), State) -> {{Type, Line, Val}, State};
ast_to_ast(?V(Line, float=Type, Val), State)   -> {{Type, Line, Val}, State};
ast_to_ast(?V(Line, boolean, Val), State)      -> {{atom, Line, Val}, State};
ast_to_ast(?V(Line, var=Type, Val), State)     -> {{Type, Line, Val}, State};
ast_to_ast(?V(Line, string=Type, Val), State)  -> {{Type, Line, Val}, State};
ast_to_ast(?V(Line, bstring, Val), State) ->
    R = {bin, Line, [{bin_element, 5, {string, Line, Val}, default, default}]},
    {R, State};

ast_to_ast(?UO(Line, Op, Val), State) ->
    {EVal, State1} = ast_to_ast(Val, State),
    R = {op, Line, map_op(Op), EVal},
    {R, State1};

ast_to_ast(Ast, State) ->
    Line = element(2, Ast),
    State1 = add_error(State, invalid_expression, Line, {ast, Ast}),
    R = {atom, Line, error},
    {R, State1}.


ast_to_ast([], Accum, State) ->
    {lists:reverse(Accum), State};
ast_to_ast([H|T], Accum, State) ->
    {EH, State1} = ast_to_ast(H, State),
    ast_to_ast(T, [EH|Accum], State1).

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

list_to_cons_list(Line, Val, State) ->
    list_to_cons_list_r(Line, lists:reverse(Val), {nil, Line}, State).

list_to_cons_list_r(_Line, [], Cons, State) ->
    {Cons, State};

list_to_cons_list_r(Line, [H|T], Cons, State) ->
    {EH, State1} = ast_to_ast(H, State),
    list_to_cons_list_r(Line, T, {cons, Line, EH, Cons}, State1).

ast_to_export_fun(?O(_Line, '/', ?V(_ALine, atom, FunName), ?V(_ArLine, integer, Arity)), State) ->
    R = {FunName, Arity},
    {R, State};
ast_to_export_fun(Ast, State) ->
    Line = element(2, Ast),
    State1 = add_error(State, invalid_export, Line,
                       expected_got("funname/Arity", {ast, Ast})),
    {{atom, Line, error}, State1}.

ast_to_catch({cmatch, Line, {[Match], When, Body}}, State) ->
    cmatch_to_catch(Line, ?V(Line, atom, throw), Match, When, Body, State);
ast_to_catch({cmatch, Line, {[?V(_ALine, atom, throw=_ClassName)=CN, Match], When, Body}}, State) ->
    cmatch_to_catch(Line, CN, Match, When, Body, State);
ast_to_catch({cmatch, Line, {[?V(_ALine, atom, error=_ClassName)=CN, Match], When, Body}}, State) ->
    cmatch_to_catch(Line, CN, Match, When, Body, State);
ast_to_catch({cmatch, Line, {[?V(_ALine, atom, exit=_ClassName)=CN, Match], When, Body}}, State) ->
    cmatch_to_catch(Line, CN, Match, When, Body, State);
ast_to_catch({cmatch, Line, {[?V(_ALine, var, _VarName)=Var, Match], When, Body}}, State) ->
    cmatch_to_catch(Line, Var, Match, When, Body, State);
ast_to_catch({celse, Line, Body}, State) ->
    EMatch = {tuple, Line, [{var, Line, '_'}, {var, Line, '_'}, {var, Line, '_'}]},
    {EBody, State1} = ast_to_ast(Body, State),
    R = {clause, Line, [EMatch], [], EBody},
    {R, State1};
ast_to_catch({cmatch, Line, {Match, _When, _Body}}, State) ->
    State1 = add_error(State, invalid_catch, Line,
                       expected_got("throw:T, error:E, exit:X or T",
                                    {ast, ?S(Line, tuple, Match)})),
    {{atom, Line, error}, State1}.

cmatch_to_catch(Line, Class, Match, When, Body, State) ->
    {EClass, State1} = ast_to_ast(Class, State),
    {EMatch, State2} = ast_to_ast(Match, State1),
    ETupleMatch = {tuple, Line, [EClass, EMatch, {var, Line, '_'}]},
    {EBody, State3} = ast_to_ast(Body, State2),
    {EWhen, State4} = when_to_ast(When, State3),
    R = {clause, Line, [ETupleMatch], EWhen, EBody},
    {R, State4}.

when_to_ast(nowhen, State) -> {[], State};
when_to_ast(When, State) when is_list(When) ->
    {R, State1} = state_map(fun when_to_ast/2, When, State),
    {lists:reverse(R), State1};
when_to_ast(When, State) ->
    ast_to_ast(When, State).

kv_to_ast(Key, Val, State) ->
    {EKey, State1} = ast_to_ast(Key, State),
    {EVal, State2} = ast_to_ast(Val, State1),
    {EKey, EVal, State2}.

to_map_field({kv, Line, Key, Val}, State) ->
    {EKey, EVal, State1} = kv_to_ast(Key, Val, State),
    R = {map_field_assoc, Line, EKey, EVal},
    {R, State1};
to_map_field({kvmatch, Line, Key, Val}, State) ->
    {EKey, EVal, State1} = kv_to_ast(Key, Val, State),
    R = {map_field_exact, Line, EKey, EVal},
    {R, State1}.

to_record_field({kv, Line, Key, Val}, State) ->
    {EKey, EVal, State1} = kv_to_ast(Key, Val, State),
    R = {record_field, Line, EKey, EVal},
    {R, State1}.

to_record_field_decl(?O(Line, '=', ?V(FLine, atom, FieldName), Val), State) ->
    {EVal, State1} = ast_to_ast(Val, State),
    R = {record_field, Line, {atom, FLine, FieldName}, EVal},
    {R, State1};
to_record_field_decl(?V(Line, 'atom', FieldName), State) ->
    R = {record_field, Line, {atom, Line, FieldName}},
    {R, State};
to_record_field_decl(Other, State) ->
    Line = element(2, Other),
    State1 = add_error(State, bad_record_field_decl, Line,
                       expected_got("atom or assignment", {ast, Other})),
    {{atom, Line, error}, State1}.

% erlang ast
% NOTE for now empty case in switch matches the empty tuple
to_tuple_clause({clause, Line, [], Guard, Body}) ->
    {clause, Line, [{tuple, Line, []}], Guard, Body};
to_tuple_clause({clause, _Line, [_Match], _Guard, _Body}=Ast) ->
    Ast;
to_tuple_clause({clause, Line, Matches, Guard, Body}) ->
    {clause, Line, [{tuple, Line, Matches}], Guard, Body}.

for_qualifier_to_ast({filter, Ast}, State) -> ast_to_ast(Ast, State);
for_qualifier_to_ast({bgenerate, Line, Left, Right}, State) ->
    {ELeft, ERight, State1} = kv_to_ast(Left, Right, State),
    R = {b_generate, Line, ELeft, ERight},
    {R, State1};
for_qualifier_to_ast({generate, Line, Left, Right}, State) ->
    {ELeft, ERight, State1} = kv_to_ast(Left, Right, State),
    R = {generate, Line, ELeft, ERight},
    {R, State1}.

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

state_map(Fun, Seq, State) ->
    lists:foldl(fun (Item, {Accum, StateIn}) ->
                       {R, State1} = Fun(Item, StateIn),
                       {[R|Accum], State1}
               end,  {[], State}, Seq).

add_attributes(#{attrs := AttrList}=State, Type, Line, Name, Attrs) ->
    NewAttrList = [{Type, Line, Name, Attrs}|AttrList],
    State#{attrs => NewAttrList}.

expected_got(Expected, Got) -> {expected, Expected, got, Got}.

check_case_arities_equal([{cmatch, Line, {Cond, _When, _Body}}|T], State, Arity) ->
    CaseArity = length(Cond),
    if CaseArity == Arity -> check_case_arities_equal(T, State, Arity);
       true ->
           State1 = add_error(State, case_mismatch, Line,
                              expected_got(Arity, CaseArity)),
           check_case_arities_equal(T, State1, Arity)
    end;
check_case_arities_equal([{celse, _Line, _Body}|T], State, Arity) ->
    check_case_arities_equal(T, State, Arity);
check_case_arities_equal([], State, _Arity) -> State.

lc_to_ast(Line, Qualifiers, Body, State) ->
    {EBody, State1} = case Body of
                          [Node] -> ast_to_ast(Node, State);
                              Nodes ->
                              {EBlockBody, S1} = ast_to_ast(Nodes, State),
                              Ri = {block, Line, EBlockBody},
                              {Ri, S1}
                      end,
    {Items, State2} = state_map(fun for_qualifier_to_ast/2, Qualifiers, State1),
    {lists:reverse(Items), EBody, State2}.

info_to_ast(Line, "line", State) ->
    {{integer, Line, Line}, State};
info_to_ast(Line, "module", #{module := Module}=State) ->
    {{atom, Line, Module}, State};
info_to_ast(Line, Name, State) ->
    State1 = add_error(State, unknown_compiler_info, Line,
                       expected_got("\"line\" or \"module\"", Name)),
    {{atom, Line, error}, State1}.

add_bin_element_param(default, Param, State) ->
    add_bin_element_param([], Param, State);
add_bin_element_param(L, Param, State) ->
    {[Param|L], State}.

add_bin_element_param(Line, Params, Param, ValidValues, State) ->
    IsInValues = lists:member(Param, ValidValues),
    if IsInValues -> add_bin_element_param(Params, Param, State);
       true ->
           Msg = io_lib:format("one of ~p", [ValidValues]),
           State1 = add_error(State, invalid_bin_type_specifier_value, Line,
                              expected_got(Msg, Param)),
           {Params, State1}
    end.

parse_bin_element_fields(_Line, [], State, BinElement) ->
    {BinElement, State};
parse_bin_element_fields(Line, [{kv, _Line, ?Atom(val), ?V(_, var, _VarName)=NewName}|T],
                         State, {BeType, BeLine, _OldName, Size, Params}) ->
    {ENewName, State1} = ast_to_ast(NewName, State),
    NewBinElement = {BeType, BeLine, ENewName, Size, Params},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, _Line, ?Atom(size), ?V(_, integer, _Size)=NewSize}|T],
                         State, {BeType, BeLine, BeName, _OldSize, Params}) ->
    {ENewSize, State1} = ast_to_ast(NewSize, State),
    NewBinElement = {BeType, BeLine, BeName, ENewSize, Params},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, _Line, ?Atom(unit), ?V(_, integer, Unit)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params})
                        when Unit >= 1 andalso Unit =< 256 ->
    {NewParams, State1} = add_bin_element_param(Params, {unit, Unit}, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(type), ?Atom(Type)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [integer, float, binary, bytes, bitstring, bits, utf8, utf16, utf32],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Type, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(endianness), ?Atom(Endianness)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [big, little, native],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Endianness, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [{kv, KvLine, ?Atom(sign), ?Atom(Sign)}|T],
                         State, {BeType, BeLine, BeName, BeSize, Params}) ->
    ValidValues = [signed, unsigned],
    {NewParams, State1} = add_bin_element_param(KvLine, Params, Sign, ValidValues, State),
    NewBinElement = {BeType, BeLine, BeName, BeSize, NewParams},
    parse_bin_element_fields(Line, T, State1, NewBinElement);

parse_bin_element_fields(Line, [Other|T], State, BinElement) ->
    Msg = "one of val (var), size (integer), type (atom), sign (atom), endianness (atom), unit (1..256)",
    OtherLine = element(2, Other),
    State1 = add_error(State, invalid_bin_type_specifier_field, OtherLine,
                       expected_got(Msg, {ast, Other})),
    parse_bin_element_fields(Line, T, State1, BinElement).

to_bin_element(?S(Line, map, Fields), State) ->
    InitialState =  {bin_element, Line, {var, Line, '_'}, default, default},
    parse_bin_element_fields(Line, Fields, State, InitialState);

to_bin_element(Other, State) ->
    Line = element(2, Other),
    State1 = add_error(State, invalid_bin_type_specifier, Line,
                       expected_got("\"line\" or \"module\"", Other)),
    {{atom, Line, error}, State1}.


type_specifiers_to_ast(Line, TSList, State) ->
    {RFields, State1} = lists:mapfoldl(fun to_bin_element/2, State, TSList),
    R = {bin, Line, RFields},
    {R, State1}.

add_error(#{errors:=Errors}=State, ErrType, Line, Detail) ->
    Error = {ErrType, Line, Detail},
    NewErrors = [Error|Errors],
    State#{errors => NewErrors}.

unwrap(?V(_Line, _Type, Val)) -> Val.
