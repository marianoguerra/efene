-module(efene).
-compile(export_all).

module(Name, Ast) ->
    [{attribute,1,module,Name},
        {attribute,1,compile,export_all}] ++ Ast.

function(Name, Line, Arity, Ast) ->
    {function, Line, Name, Arity, Ast}.

function_body(Pattern, Line, Guard, Body) ->
    {clause, Line, Pattern, Guard, Body}.

forms('=', Line, Ast1, Ast2) ->
  {match, Line, Ast1, Ast2};
forms('not' = Op, Line, Ast1, nil) ->
  {op, Line, Op, Ast1};
forms('-' = Op, Line, Ast1, nil) ->
  {op, Line, Op, Ast1};
forms('~', Line, Ast1, nil) ->
  {op, Line, 'bnot', Ast1};
forms('|', Line, Ast1, Ast2) ->
  {op, Line, 'bor', Ast1, Ast2};
forms('!'=Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('&', Line, Ast1, Ast2) ->
  {op, Line, 'band', Ast1, Ast2};
forms('^', Line, Ast1, Ast2) ->
  {op, Line, 'bxor', Ast1, Ast2};
forms('<<', Line, Ast1, Ast2) ->
  {op, Line, 'bsl', Ast1, Ast2};
forms('>>', Line, Ast1, Ast2) ->
  {op, Line, 'bsr', Ast1, Ast2};
forms('++' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('--' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
% the code below is from reia (I would have written the same :P)
% changes on == and != and parameter order
forms('*' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('/' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('%', Line, Ast1, Ast2) ->
  {op, Line, 'rem', Ast1, Ast2};

%% Addition
forms('+' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('-' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};

%% Boolean operators
forms('and', Line, Ast1, Ast2) ->
  {op, Line, 'andalso', Ast1, Ast2};
forms('or', Line, Ast1, Ast2) ->
  {op, Line, 'orelse', Ast1, Ast2};

%% Comparison operators
forms('==', Line, Ast1, Ast2) ->
  {op, Line, '==', Ast1, Ast2};
forms('===', Line, Ast1, Ast2) ->
  {op, Line, '=:=', Ast1, Ast2};
forms('!=', Line, Ast1, Ast2) ->
  {op, Line, '/=', Ast1, Ast2};
forms('!==', Line, Ast1, Ast2) ->
  {op, Line, '=/=', Ast1, Ast2};
forms('<' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('>' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('>=' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('<=', Line, Ast1, Ast2) ->
  {op, Line, '=<', Ast1, Ast2}.
% until here code from reia

get_ast(From, String) ->
    Ast = lists:map(fun(Line) -> matches(Line) end, get_tree(From, String)),
    module(get_module_name(String), Ast).

print_ast(From, String) ->
    io:format("~p~n", [get_ast(From, String)]).

get_code(Ast) ->
    {ok, _, Code} = compile:forms(Ast),
    Code.

build_module(ModuleName, Ast) ->
    {module, Module} = code:load_binary(ModuleName, ModuleName, get_code(Ast)),
    Module.

to_erlang(From, String) ->
    Ast = get_ast(From, String),
    erl_prettypr:format(erl_syntax:form_list(Ast)).

print_erlang([File]) ->
    print_erlang(file, File).

print_erlang(From, String) ->
    io:format("~s~n", [to_erlang(From, String)]).

from_erlang(Name) ->
    {ok, Content} = file:read_file(Name),
    Program = binary_to_list(Content),
    {ok,Scanned,_} = erl_scan:string(Program),
    {ok,Parsed} = erl_parse:parse_form(Scanned),
    Parsed.

print_from_erlang(Name) ->
    io:format("~p~n", [from_erlang(Name)]).

build(From, String) ->
    ModuleName = get_module_name(String),
    build_module(ModuleName, get_ast(From, String)).

compile(Name) ->
    compile(Name, ".").

compile(Name, Dir) ->
    Module = get_code(flatten1level(get_ast(file, Name))),
    Path = filename:join([Dir, get_module_beam_name(Name)]),
    {ok, Device} = file:open(Path, [binary, write]),
    file:write(Device, Module).

flatten1level(List) -> flatten1level(List, []).

flatten1level([], Result) -> lists:reverse(Result);
flatten1level([Head|Tail], Result) when is_list(Head) -> flatten1level(Tail, lists:append(lists:reverse(Head), Result));
flatten1level([Head|Tail], Result) -> flatten1level(Tail, [Head|Result]).


get_tree(From, String) ->
    Tokens = get_lex(From, String),
    {ok, Tree} = parser:parse(Tokens),
    Tree.

print_tree(From, String) ->
    io:format("~p~n", [get_tree(From, String)]).

get_lex(string, String) ->
    {ok, Tokens, _Endline} = lexer:string(String),
    Tokens;
get_lex(file, Name) ->
    {ok, Content} = file:read_file(Name),
    Program = binary_to_list(Content),
    {ok, Tokens, _Endline} = lexer:string(Program),
    Tokens.

print_lex(From, String) ->
    io:format("~p~n", [get_lex(From, String)]).

get_module_name(String) ->
    File = filename:basename(String),
    ModuleNameStr  = filename:rootname(File),
    list_to_atom(ModuleNameStr).

get_module_beam_name(String) ->
    File = filename:basename(String),
    ModuleNameStr  = filename:rootname(File),
    string:concat(ModuleNameStr, ".beam").

matches([]) -> [];
matches([_|_] = List) -> matches_list(List);
matches({integer, _, _} = Ast) -> Ast;
matches({float, _, _} = Ast) -> Ast;
matches({atom, _, _} = Ast) -> Ast;
matches({string, _, _} = Ast) -> Ast;
matches({var, _, _} = Ast) -> Ast;
matches({nil, _} = Ast) -> Ast;
matches(default) -> default;
matches({cons, Line, A, B}) -> {cons, Line, matches(A), matches(B)};
matches({tuple, Line, A}) -> {tuple, Line, matches_list(A)};
matches({'+' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'-' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'*' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'/' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'%' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({'++' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'--' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({'<<' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'>>' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({'<' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'<=' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'==' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'>=' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'>' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'!=' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({'!' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({'|' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'&' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'^' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({'and' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({'or' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({'not' = Op, Line, A}) -> forms(Op, Line, matches(A), nil);
matches({'~' = Op, Line, A}) -> forms(Op, Line, matches(A), nil);
matches({'+', _Line, A}) -> matches(A);
matches({'-' = Op, Line, A}) -> forms(Op, Line, matches(A), nil);
matches({'(', _Line, A}) -> matches(A);

matches({'bin'=Op, Line, BinElements}) -> {Op, Line, matches_list(BinElements)};
matches({'bin_element'=Op, Line, Value, Size, Types}) -> {Op, Line, matches(Value), matches(Size), Types};

matches({lc=Op, Line, Exp, Generators}) -> {Op, Line, matches(Exp), matches_list(Generators)};
matches({generate=Op, Line, For, In}) -> {Op, Line, matches(For), matches(In)};
matches({generate=Op, Line, For, In, If}) -> {Op, Line, matches(For), matches(In), matches(If)};

matches({bc=Op, Line, Exp, Generators}) -> {Op, Line, matches(Exp), matches_list(Generators)};
matches({b_generate=Op, Line, For, In}) -> {Op, Line, matches(For), matches(In)};
matches({b_generate=Op, Line, For, In, If}) -> {Op, Line, matches(For), matches(In), matches(If)};
matches({callatom, Line, [Atom], Args}) ->
    {call, Line, Atom, matches_list(Args)};
matches({callatom, Line, [Package, Function], Args}) ->
    {call, Line, {remote, Line, Package, Function}, matches_list(Args)};
matches({call, Line, A, Args}) ->
    {call, Line, matches(A), matches_list(Args)};
matches({'=' = Op, Line, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({fn, Line, Patterns}) ->
    {'fun', Line, match_fun_body(Patterns)};
matches({fun_def, Line, Name, {fn, _Line, Patterns}}) ->
    function(Name, Line, get_function_arity(Patterns), match_function_body(Patterns));
matches({obj_def, Line, Name, Fields}) ->
    build_record(Name, Fields, Line);

matches({receives, Line, Patterns}) -> {'receive', Line, match_function_body(Patterns)};
matches({receives, Line, Patterns, After, {'{', _, AfterBody}}) -> {'receive', Line, match_function_body(Patterns), matches(After), matches(AfterBody)};

matches({trys, Line, {'{', _, TryBody}, CatchPatterns}) ->
    {'try', Line, matches(TryBody), [], match_function_body(CatchPatterns), []};
matches({trys, Line, {'{', _, TryBody}, CatchPatterns, {'{', _, FinallyBody}}) ->
    {'try', Line, matches(TryBody), [], match_function_body(CatchPatterns), matches(FinallyBody)};

matches(Exp) -> {error, Exp}.

matches_list([]) -> [];
matches_list(Items) -> matches_list(Items, []).

matches_list([], Accum) -> lists:reverse(Accum);
matches_list([Head | Tail], Accum) -> matches_list(Tail, [matches(Head) | Accum]).

match_fun_body(Patterns) ->
    {clauses, match_function_body(Patterns)}.

match_function_body(Patterns) -> match_function_body(Patterns, []).

match_function_body([], Clauses) -> lists:reverse(Clauses);
match_function_body([Pattern | Patterns], Clauses) ->
     match_function_body(Patterns, [match_pattern(Pattern) | Clauses]).

match_pattern({pattern, {'(', Line, Args}, [], {'{', _, Body}}) ->
     function_body([matches(Arg) || Arg <- Args], Line, [], matches_list(Body));
match_pattern({pattern, {'(', Line, Args}, Guards, {'{', _, Body}}) ->
     function_body([matches(Arg) || Arg <- Args], Line, [matches_list(Guards)], matches_list(Body)).

get_function_arity([]) -> 0;
get_function_arity([{pattern, {'(', _Line, Arguments}, _, _}|_T]) -> length(Arguments).

build_record_get(RecordName, FieldName, Line) ->
    {function, Line, FieldName, 1,
        [{clause, Line, [{var,Line,'Record'}], [],
            [{record_field, Line, {var, Line, 'Record'}, RecordName, {atom,Line,FieldName}}]}]}.

build_record_set(RecordName, FieldName, Line) ->
    {function,Line,FieldName,2,
        [{clause, Line,
             [{var, Line, 'Record'},{var, Line, 'Value'}], [],
             [{record, Line, {var, Line, 'Record'}, RecordName,
                  [{record_field, Line,{atom, Line, FieldName}, {var, Line, 'Value'}}]}]}]}.

build_record_get_and_set(RecordName, FieldName, Line) ->
    [build_record_get(RecordName, FieldName, Line),
        build_record_set(RecordName, FieldName, Line + 2)].


build_record_new(RecordName, FieldNames, Line) ->
    NextLine = Line + 1,
    {function, Line, new, length(FieldNames),
        [{clause, Line,
            [{var, Line, first_upper(FieldName)} || FieldName <- FieldNames],
             [],
             [{record,NextLine,RecordName,
                 [{record_field, NextLine, {atom, NextLine, FieldName}, {var, NextLine, first_upper(FieldName)}}
                    || FieldName <- FieldNames]}]}]}.

build_record_keys(FieldNames, Line) ->
    {function,Line,keys,0,
        [{clause, Line,
             [], [],
             [{tuple, Line, [{atom, Line, FieldName} || FieldName <- FieldNames]}]}]}.

build_record(RecordName, FieldNames, Line) ->
    Code = [{attribute, Line, record,{RecordName, [{record_field, Line, {atom, Line, FieldName}} || FieldName <- FieldNames]}},
        build_record_new(RecordName, FieldNames, Line + 2),
        build_record_keys(FieldNames, Line + 4)],
    add_record_get_and_set(RecordName, FieldNames, Line + 6, Code).

add_record_get_and_set(_RecordName, [], _Line, Code) -> lists:flatten(Code);
add_record_get_and_set(RecordName, [FieldName|FieldNames], Line, Code) ->
    add_record_get_and_set(RecordName, FieldNames, Line + 4, [Code | build_record_get_and_set(RecordName, FieldName, Line + 2)]).

first_upper(Atom) when is_atom(Atom) -> first_upper(atom_to_list(Atom));
first_upper([Head|Tail]) -> list_to_atom([string:to_upper(Head)] ++ Tail).

% command line functions

main([Dir, File]) ->
    try
        io:format("compiling ~s~n", [File]),
        compile(File, Dir)
    catch
        _:Error ->
            display_error(Error)
    end.

display_error({badmatch,{error,{Line,lexer,{illegal,Character}},_}}) ->
    io:format("~B: Illegal character '~s'~n", [Line, Character]);
display_error({badmatch,{error,{Line,parser,[Message,Item]}}}) ->
    io:format("~B: ~s~s~n", [Line, Message, Item]);
display_error(Unknown) ->
    io:format("Error: ~p~n", [Unknown]).


