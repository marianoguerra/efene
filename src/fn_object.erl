-module(fn_object).
-export([to_ast/3]).

to_ast(Line, Name, Fields) ->
    RecordDef = fn_gen:record(Line, Name, Fields),
    FromRecordArg = fn_gen:record_match(Line, Name, Fields),
    FromRecordArgLen = 1,

    FromFieldsArgs = fn_gen:vars_from_fields(Line, Fields),
    FromFieldsArgsLen = length(FromFieldsArgs),

    %person(#person{firstname=FirstName, lastname=LastName, mail=Mail}) ->
    %    person(FirstName, LastName, Mail).
    FromRecord = fn_gen:func(Line, Name, FromRecordArgLen,
        [fn_gen:clause(Line, [FromRecordArg],
            [fn_gen:call(Line, Name, FromFieldsArgs)])]),

    ObjVar = fn_gen:var(Line, 'Obj'),
    WrapperVar = fn_gen:var(Line, 'Wrapper'),

    BuildRecord = fn_gen:match(Line, ObjVar, FromRecordArg),

    Wrapper = wrapper(Line, Name, Fields),
    BuildWrapper = fn_gen:match(Line, WrapperVar, Wrapper),

    CallWrapper = fn_gen:call_var(Line, 'Wrapper', [WrapperVar, ObjVar]),

    FromFields = fn_gen:func(Line, Name, FromFieldsArgsLen,
        [fn_gen:clause(Line, FromFieldsArgs,
            [BuildRecord, BuildWrapper, CallWrapper])]),

    [FromFields, FromRecord, RecordDef].

wrapper(Line, Name, Fields) ->
    VarName = fn_gen:atom_to_upper(Name),

    Gets = [get_field(Line, Name, VarName, Field) || Field <- Fields],
    Sets = [set_field(Line, Name, VarName, Field) || Field <- Fields],
    Sett = [set_field_tuple(Line, Name, VarName, Field) || Field <- Fields],
    Specials = specials(Line, Name, Fields),

    fn_gen:fun_(Line,
        [fn_gen:clause(Line, [fn_gen:var(Line, 'Self'), fn_gen:var(Line, VarName)],
                [fn_gen:fun_(Line, Gets ++ Sets ++ Sett ++ Specials)])]).

get_field(Line, RecName, VarName, FieldName) ->
    fn_gen:clause(Line, [fn_gen:atom(Line, get), fn_gen:atom(Line, FieldName)],
        [fn_gen:record_field_get(Line, RecName, VarName, FieldName)]).

set_field(Line, RecName, VarName, FieldName) ->
    Set = list_to_atom("set" ++ atom_to_list(FieldName)),

    fn_gen:clause(Line, [fn_gen:atom(Line, Set), fn_gen:var(Line, 'Value')],
        [fn_gen:call_var(Line, 'Self', [fn_gen:var(Line, 'Self'),
            fn_gen:record_field_set(Line, RecName, VarName, FieldName,
                'Value')])]).

set_field_tuple(Line, RecName, VarName, FieldName) ->
    fn_gen:clause(Line, [fn_gen:tuple(Line, [fn_gen:atom(Line, 'set'), fn_gen:atom(Line, FieldName)]), fn_gen:var(Line, 'Value')],
        [fn_gen:call_var(Line, 'Self', [fn_gen:var(Line, 'Self'),
            fn_gen:record_field_set(Line, RecName, VarName, FieldName,
                'Value')])]).

specials(Line, Name, Fields) ->
    Has = [fn_gen:clause(Line,
            [fn_gen:atom(Line, has), fn_gen:atom(Line, Field)],
            [fn_gen:true(Line)]) || Field <- Fields],

    HasStr = [fn_gen:clause(Line,
            [fn_gen:atom(Line, has), fn_gen:string(Line, atom_to_list(Field))],
            [fn_gen:true(Line)]) || Field <- Fields],

    HasNotFound = fn_gen:clause(Line,
        [fn_gen:atom(Line, has), fn_gen:var(Line, '_')],
        [fn_gen:false(Line)]),

    NotFound = fn_gen:clause(Line,
        [fn_gen:var(Line, 'A'), fn_gen:var(Line, 'B')],
        [fn_gen:call(Line, throw,
                [fn_gen:tuple(Line, [fn_gen:atom(Line, method_not_found),
                            fn_gen:var(Line, 'A'), fn_gen:var(Line, 'B')])])]),

    ToRecord = fn_gen:clause(Line,
        [fn_gen:atom(Line, to), fn_gen:atom(Line, rec)],
        [fn_gen:var(Line, fn_gen:atom_to_upper(Name))]),

    ToFields = fn_gen:clause(Line,
        [fn_gen:atom(Line, to), fn_gen:atom(Line, fields)],
        [fn_gen:tuple(Line, [fn_gen:atom(Line, Field) || Field <- Fields])]),

    ToFieldsList = fn_gen:clause(Line,
        [fn_gen:atom(Line, to), fn_gen:atom(Line, fieldslist)],
        [fn_gen:literal_to_ast(Fields, Line)]),

    ToName = fn_gen:clause(Line,
        [fn_gen:atom(Line, to), fn_gen:atom(Line, name)],
        [fn_gen:atom(Line, Name)]),

    ToStringName = fn_gen:clause(Line,
        [fn_gen:atom(Line, to), fn_gen:atom(Line, strname)],
        [fn_gen:string(Line, atom_to_list(Name))]),

    [ToRecord, ToFields, ToFieldsList, ToName, ToStringName|Has] ++ HasStr ++ [HasNotFound, NotFound].
