-module(fn_gen).
-compile(export_all).

function(Line, Name, Args, Body) ->
    {function, Line, Name, length(Args),
        [{clause, Line, Args, [], Body}]}.

literal_to_ast(Val, Line) when is_tuple(Val) ->
    {tuple, Line,
        [literal_to_ast(V, Line) || V <- tuple_to_list(Val)]};
literal_to_ast(Val, Line) when is_boolean(Val) ->
    {boolean, Line, Val};
literal_to_ast(Val, Line) when is_integer(Val) ->
    {integer, Line, Val};
literal_to_ast(Val, Line) when is_float(Val) ->
    {float, Line, Val};
literal_to_ast(Val, Line) when is_atom(Val) ->
    {atom, Line, Val};
literal_to_ast([], Line) ->
    {nil, Line};
literal_to_ast([H|T], Line) ->
    {cons, Line, literal_to_ast(H, Line), literal_to_ast(T, Line)}.

% record declaration
record(Line, Name, Fields) ->
    {attribute, Line, record, {Name, record_fields(Line, Fields)}}.

record_fields(Line, Fields) ->
    [record_field(Line, Field) || Field <- Fields].

record_field(Line, Field) ->
    {record_field, Line, {atom, Line, Field}}.

% match all the fields to variables with the same name
% example: #person{firstname=FirstName, lastname=LastName, mail=Mail}
record_match(Line, Name, Fields) ->
    {record, Line, Name, record_fields_match(Line, Fields)}.

record_fields_match(Line, Fields) ->
    [record_field_match(Line, Field) || Field <- Fields].

record_field_match(Line, Field) ->
    {record_field, Line, {atom, Line, Field}, {var, Line, atom_to_upper(Field)}}.

record_field_match(Line, Field, Var) ->
    {record_field, Line, {atom, Line, Field}, {var, Line, Var}}.

record_field_get(Line, RecName, VarName, FieldName) ->
    {record_field, Line, {var, Line, VarName}, RecName, {atom, Line, FieldName}}.

record_field_set(Line, RecName, VarName, FieldName, NewValueName) ->
    {record, Line, {var, Line, VarName}, RecName,
        [record_field_match(Line, FieldName, NewValueName)]}.

atom_to_upper(Atom) when is_atom(Atom) ->
    atom_to_upper(atom_to_list(Atom));
atom_to_upper([H|T]) ->
    list_to_atom([string:to_upper(H)|T]).

func(Line, Name, Arity, Clauses) ->
    {function, Line, Name, Arity, Clauses}.

fun_(Line, Clauses) ->
    {'fun', Line, {clauses, Clauses}}.

clause(Line, Args, Body) ->
    {clause, Line, Args, [], Body}.

call(Line, Name, Args) ->
    {call, Line, {atom, Line, Name}, Args}.

call(Line, Module, Name, Args) ->
    {call, Line, {remote, {atom, Line, Module}, {atom, Line, Name}}, Args}.

call_var(Line, Name, Args) ->
    {call, Line, {var, Line, Name}, Args}.

vars_from_fields(Line, Fields) ->
    [var_from_field(Line, Field) || Field <- Fields].

var_from_field(Line, Atom) ->
    {var, Line, atom_to_upper(Atom)}.

var(Line, Name) ->
    {var, Line, Name}.

atom(Line, Name) ->
    {atom, Line, Name}.

true(Line) ->
    {atom, Line, true}.

false(Line) ->
    {atom, Line, false}.

tuple(Line, Items) ->
    {tuple, Line, Items}.

match(Line, Left, Right) ->
    {match, Line, Left, Right}.
