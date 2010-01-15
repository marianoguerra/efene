-module(fn_record).
-compile(export_all).

% TODO: use the other functions to make this code simpler
%  (call a function instead of hardcoding the tuples)

% convert an atom to a string with the first char as uppercase
first_upper(Atom) when is_atom(Atom) -> first_upper(atom_to_list(Atom));
first_upper([Head|Tail]) -> list_to_atom([string:to_upper(Head)] ++ Tail).

% convert a list of atoms representing the fields of a record
% to a list of arguments for the inner fun
fields_to_args(Line, Fields) ->
    [{var, Line, first_upper(Field)} || Field <- Fields].

% create the record instantiation
fields_to_record(Line, Name, Fields) ->
    {record,Line,Name,
        [{record_field,Line,{atom,Line,Field},{var,Line,first_upper(Field)}} || Field <- Fields]}.

make_get_name(Name) -> list_to_atom("get" ++ atom_to_list(Name)).

make_set_name(Name) -> list_to_atom("set" ++ atom_to_list(Name)).

build_getters(Line, Name, VarName, Fields) ->
    [{clause,Line,
        [{atom,Line,make_get_name(Field)}], [],
        [{record_field,Line,{var,Line,VarName},Name,{atom,5,Field}}]} || Field <- Fields].

build_setters(Line, Name, VarName, Fields) ->
    [{clause,Line,
     [{tuple,Line,[{atom,Line,make_set_name(Field)},{var,Line,'Value'}]}],
     [],
     [{call,Line,
     {var,Line,'Self'},
     [{var,Line,'Self'},
      {record,Line,
       {var,Line,VarName},
       Name,
       [{record_field,Line,{atom,Line,Field},{var,Line,'Value'}}]}]}]} || Field <- Fields].

% build an expressions that means a list of atoms
build_list_of_atoms([Last], Line) -> {cons, Line, {atom, Line, Last}, {nil, Line}};
build_list_of_atoms([Head|Tail], Line) -> {cons, Line, {atom, Line, Head}, build_list_of_atoms(Tail, Line)}.

build_common_functions(Line, Name, VarName, Fields) ->
         [{clause,Line,
          [{tuple,Line,[{atom,Line,has},{var,Line,'Field'}]}],
          [],
          [{call,Line,
            {remote,Line,{atom,Line,lists},{atom,Line,member}},
            [{var,Line,'Field'}, build_list_of_atoms(Fields, Line)]}]},
         {clause,Line,[{atom,Line,record}],[],[{var,Line,VarName}]},
         {clause,Line,
          [{atom,Line,fields}],
          [],
          [{tuple,Line,
            [{atom, Line, Field} || Field <- Fields]}]},
         {clause,Line,[{atom,Line,name}],[],[{atom,Line,Name}]}].

build(Line, Name, Fields) ->
    VarName = first_upper(Name),
    Methods = build_getters(Line, Name, VarName, Fields) ++ build_setters(Line, Name, VarName, Fields) ++ build_common_functions(Line, Name, VarName, Fields),
    [{attribute, Line, record,{Name, [{record_field, Line, {atom, Line, Field}} || Field <- Fields]}},
    {function,Line,Name,length(Fields),
     [{clause,Line,
        fields_to_args(Line, Fields),
       [],
       [{match,Line,
         {var,Line,'Obj'},
        fields_to_record(Line, Name, Fields)},
        {match,Line,
         {var,Line,'Wrapper'},
         {'fun',Line,
          {clauses,
           [{clause,Line,
             [{var,Line,'Self'},{var,Line,VarName}],
             [],
             [{'fun',Line,
               {clauses, Methods}}]}]}}},
        {call,Line,{var,Line,'Wrapper'},[{var,Line,'Wrapper'},{var,Line,'Obj'}]}]}]}].

