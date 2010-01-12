-module(efene).
-compile(export_all).

get_ast(From, String) ->
    Ast = lists:map(fun(Line) -> fn_match:match(Line) end, get_tree(From, String)),
    fn_gen:module(get_module_name(String), flatten1level(Ast)).

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
    %[Last|Head] = lists:reverse(Ast),
    %io:format("tuple: ~p, list: ~p~n", [is_tuple(Last),
    %        is_list(Last)]),
    %erl_prettypr:format(Ast).
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
    Module = get_code(get_ast(file, Name)),
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

% command line functions

main(["beam", Dir, File]) ->
    try
        io:format("compiling ~s~n", [File]),
        compile(File, Dir)
    catch
        _:Error ->
            display_error(Error)
    end;
main(["ast", _Dir, File]) ->
    print_ast(file, File);
main(["tree", _Dir, File]) ->
    print_tree(file, File);
main(["lex", _Dir, File]) ->
    print_lex(file, File);
main(["erl", _Dir, File]) ->
    print_erlang([File]);
main(["erl2ast", _Dir, File]) ->
    print_from_erlang(File).

display_error({badmatch,{error,{Line,lexer,{illegal,Character}},_}}) ->
    io:format("~B: Illegal character '~s'~n", [Line, Character]);
display_error({badmatch,{error,{Line,parser,[Message,Item]}}}) ->
    io:format("~B: ~s~s~n", [Line, Message, Item]);
display_error(Unknown) ->
    io:format("Error: ~p~n", [Unknown]).


