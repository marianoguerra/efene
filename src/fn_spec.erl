-module(fn_spec).
-export([type_to_spec/2]).

-include("efene.hrl").

type_to_spec({attr, Line, ?Type(_), [?Atom(TName)], Result}, State) ->
    {Value, State1} = parse_type_value(Result, State),
    A = {attribute, Line, type, {TName, Value, []}},
    {A, State1};
type_to_spec({attr, Line, _, _, _}=Ast, State) ->
    State1 = fn_to_erl:add_error(State, invalid_type_declaration, Line, {ast, Ast}),
    R = {atom, Line, error},
    {R, State1}.

parse_type_value(?V(Line, atom=Type, Val), State)    -> {{Type, Line, Val}, State};
parse_type_value(?V(Line, integer=Type, Val), State) -> {{Type, Line, Val}, State};
parse_type_value(?V(Line, boolean, Val), State)      -> {{atom, Line, Val}, State};
parse_type_value(?E(Line, call,
                    {[?Atom(range=Name)], [?Int(FL, From), ?Int(TL, To)]}),
                 State) ->
    {{type, Line, Name, [{integer, FL, From}, {integer, TL, To}]}, State};
parse_type_value(?E(Line, call, {[?Atom(Name)], []=Args}), State) ->
    {{type, Line, Name, Args}, State};
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
    lists:foldl(Fun, {[], State}, Types).

flatten_or(?O(_Line, 'or', Left, Right), Accum) ->
    flatten_or(Right, [Left|Accum]);
flatten_or(Other, Accum) ->
    [Other|Accum].
