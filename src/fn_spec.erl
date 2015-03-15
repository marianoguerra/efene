-module(fn_spec).
-export([type_to_spec/2, parse_spec_attr/6]).

-include("efene.hrl").

parse_spec_attr(Name, Arity, Line, Args, Return, State) ->
    {EReturn, State1} = parse_type_value(Return, State),
    {EArgs, State2} = parse_types(Args, State1),
    ESpec = {attribute, Line, spec, {{Name, Arity},
                                     [{type, Line, 'fun',
                                       [{type, Line, product, EArgs}, EReturn]}]}},
    {ESpec, State2}.


type_to_spec({attr, Line, [?Atom(Type)], [?Atom(TName)], Result}, State) ->
    {Value, State1} = parse_type_value(Result, State),
    A = {attribute, Line, Type, {TName, Value, []}},
    {A, State1};
type_to_spec({attr, Line, [?Atom(Type)], [?E(_, call, {[?Atom(TName)], TArgs})], Result}, State) ->
    {ETArgs, State1} = parse_type_args(TArgs, State, []),
    {Value, State2} = parse_type_value(Result, State1),
    A = {attribute, Line, Type, {TName, Value, ETArgs}},
    {A, State2};

type_to_spec({attr, Line, _, _, _}=Ast, State) ->
    State1 = fn_to_erl:add_error(State, invalid_type_declaration, Line, {ast, Ast}),
    R = {atom, Line, error},
    {R, State1}.

parse_type_args([], State, Accum) ->
    {lists:reverse(Accum), State};
parse_type_args([Var=?Var(_)|Args], State, Accum) ->
    {EVar, State1} = parse_type_value(Var, State),
    parse_type_args(Args, State1, [EVar|Accum]);
parse_type_args([Ast|Args], State, Accum) ->
    Line = element(2, Ast),
    State1 = fn_to_erl:add_error(State, invalid_type_argument, Line, {ast, Ast}),
    parse_type_args(Args, State1, Accum).

parse_type_value(?V(Line, var=Type, Val), State)     -> {{Type, Line, Val}, State};
parse_type_value(?V(Line, atom=Type, Val), State)    -> {{Type, Line, Val}, State};
parse_type_value(?V(Line, integer=Type, Val), State) -> {{Type, Line, Val}, State};
parse_type_value(?V(Line, boolean, Val), State)      -> {{atom, Line, Val}, State};
parse_type_value(?E(Line, call,
                    {[?Atom(range=Name)], [?Int(FL, From), ?Int(TL, To)]}),
                 State) ->
    {{type, Line, Name, [{integer, FL, From}, {integer, TL, To}]}, State};
% type fun()
parse_type_value(?E(Line, call, {[?Atom('fun')], [?S(_, list, [])]}), State) ->
    {{type, Line, 'fun', []}, State};
% type fun(any, <type>)
parse_type_value(?E(Line, call, {[?Atom('fun')], [?V(ALine, atom, any), Return]}), State) ->
    {EReturn, State1} = parse_type_value(Return, State),
    {{type, Line, 'fun', [{type, ALine, any}, EReturn]}, State1};
% type fun([<type>*], <type>)
parse_type_value(?E(Line, call, {[?Atom('fun')], [?S(ALine, list, Args), Return]}), State) ->
    {EReturn, State1} = parse_type_value(Return, State),
    {EArgs, State2} = parse_types(Args, State1),
    {{type, Line, 'fun', [{type, ALine, product, EArgs}, EReturn]}, State2};
% type <atom>(<type>*)
parse_type_value(?E(Line, call, {[?Atom(Name)], Args}), State) ->
    {EArgs, State1} = parse_types(Args, State),
    {{type, Line, Name, EArgs}, State1};
parse_type_value(?S(Line, list, []), State) ->
    {{type, Line, nil, []}, State};
parse_type_value(?S(Line, list, [Type]), State) ->
    {FnLType, State1} = parse_type_value(Type, State),
    {{type, Line, list, [FnLType]}, State1};
parse_type_value(?S(Line, tuple, Types), State) ->
    {FnTypes, State1} = parse_types(Types, State),
    {{type, Line, tuple, FnTypes}, State1};
parse_type_value(?O(Line, 'or', Left, Right), State) ->
    Types = flatten_or(Right, [Left]),
    {FnTypes, State1} = parse_types(Types, State),
    {{type, Line, union, FnTypes}, State1};
parse_type_value(?LTag(Line, [?Atom(r)], ?Atom(RecordName)), State) ->
    {{type, Line, record, [{atom, Line, RecordName}]}, State};

parse_type_value(Ast, State) ->
    Line = element(2, Ast),
    State1 = fn_to_erl:add_error(State, invalid_type_value, Line, {ast, Ast}),
    R = {atom, Line, error},
    {R, State1}.

parse_types(Types, State) ->
    Fun = fun (Type, {TypesIn, StateIn}) ->
                  {FnType, StateOut} = parse_type_value(Type, StateIn),
                  TypesOut = [FnType|TypesIn],
                  {TypesOut, StateOut}
          end,
    {OutTypes, OutState} = lists:foldl(Fun, {[], State}, Types),
    {lists:reverse(OutTypes), OutState}.

flatten_or(?O(_Line, 'or', Left, Right), Accum) ->
    flatten_or(Right, [Left|Accum]);
flatten_or(Other, Accum) ->
    [Other|Accum].
