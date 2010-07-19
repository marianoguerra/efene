-module(fn_gen).
-compile(export_all).

function(Line, Name, Args, Body) ->
    {function, Line, Name, length(Args),
        [{clause, Line, Args, [], Body}]}.

literal_to_ast(Val, Line) ->
    literal_to_ast(Val, Line, false).

literal_to_ast(Val, Line, SpecialAtomToVar) when is_tuple(Val) ->
    {tuple, Line,
        [literal_to_ast(V, Line, SpecialAtomToVar) || V <- tuple_to_list(Val)]};
literal_to_ast(Val, Line, _SpecialAtomToVar) when is_boolean(Val) ->
    {atom, Line, Val};
literal_to_ast(Val, Line, _SpecialAtomToVar) when is_integer(Val) ->
    {integer, Line, Val};
literal_to_ast(Val, Line, _SpecialAtomToVar) when is_float(Val) ->
    {float, Line, Val};
literal_to_ast(Val, Line, SpecialAtomToVar) when is_atom(Val) ->
    if
        SpecialAtomToVar ->
            [FirstChar|VarName] = atom_to_list(Val),


            if
                FirstChar == $$ -> {var, Line, list_to_atom(VarName)};
                true -> {atom, Line, Val}
            end;

        true ->
            {atom, Line, Val}
    end;
literal_to_ast(Val, Line, SpecialAtomToVar) when is_list(Val) ->
    list_or_string_to_ast(Val, Line, SpecialAtomToVar).

list_or_string_to_ast(List, Line, SpecialAtomToVar) ->
    IsString = is_string(List),

    if
        IsString ->
            {string, Line, List};
        true ->
            list_to_ast(List, Line, SpecialAtomToVar)
    end.

list_to_ast([], Line, _SpecialAtomToVar) ->
    {nil, Line};
list_to_ast([H|T], Line, SpecialAtomToVar) ->
    {cons, Line, literal_to_ast(H, Line, SpecialAtomToVar), literal_to_ast(T, Line, SpecialAtomToVar)}.

is_string([]) ->
    false;
is_string(List) ->
    is_string1(List).

is_string1([]) ->
    true;
is_string1([H|T]) when is_integer(H), H >= 0 andalso H < 256 ->
    is_string1(T);
is_string1(_) ->
    false.

% record declaration
build_record(Line, Name, Fields) ->
    global_attribute(Line, record, {Name, record_fields(Line, Fields)}).

attribute(Line, Name) ->
    attribute(Line, Name, nil).

attribute(Line, Name, Args) ->
    {attribute, Line, Name, Args}.

global_attribute(Line, Name) ->
    global_attribute(Line, Name, nil).

global_attribute(Line, Name, Args) ->
    {global_attribute, Line, Name, Args}.

record_fields(Line, Fields) ->
    [record_field(Line, Field) || Field <- Fields].

public(Line) ->
    attribute(Line, public).

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

string(Line, String) ->
    {string, Line, String}.

true(Line) ->
    {atom, Line, true}.

false(Line) ->
    {atom, Line, false}.

tuple(Line, Items) ->
    {tuple, Line, Items}.

match(Line, Left, Right) ->
    {match, Line, Left, Right}.
