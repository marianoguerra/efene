-module(fn).
-export([get_lex/2,
        print_lex/2,
        get_tree/2,
        print_tree/2,
        get_ast/2,
        print_ast/2,
        get_publics/1,
        get_publics/2,
        get_attrs/1,
        get_attrs/2,
        build_module/1,
        print_module/1,
        compile/2,
        get_erlang/2,
        print_erlang/2,
        erl_to_ast/1,
        run/0,
        run/1]).

% lexer functions

get_lex(String) ->
    case fn_lexer:string(String) of
        {ok, Tokens, _Endline} -> Tokens;
        Errors -> throw(Errors)
    end.

get_lex(string, String) ->
    Lex = get_lex(String),
    fn_lexpp:clean_whites(Lex);
get_lex(istring, String) ->
    Lex1  = get_lex(String),
    Lex2 = fn_lexpp:indent_to_blocks(Lex1),
    fn_errors:fail_on_tab(Lex2);
get_lex(file, Path) ->
    Program = file_to_string(Path),

    IsFn = lists:suffix(".fn", Path),
    IsIfn = lists:suffix(".ifn", Path),

    if
        IsFn  -> get_lex(string, Program);
        IsIfn -> get_lex(istring, Program);
        true  -> exit("Invalid file extension (.fn or .ifn expected)")
    end.

print_lex(From, String) ->
    io:format("~p~n", [get_lex(From, String)]).

% tree functions
% the tree differs from the erlang absform in some small things, you should use
% ast functions for almost anything

get_tree(From, String) ->
    Tokens = get_lex(From, String),
    case fn_parser:parse(Tokens) of
        {ok, Tree}    -> Tree;
        {ok, Tree, _} -> Tree;
        {error, _Warnings, Errors} -> throw(Errors);
        Error -> throw(Error)
    end.

print_tree(From, String) ->
    io:format("~p~n", [get_tree(From, String)]).

tree_to_ast(Tree) ->
    tree_to_ast(Tree, [], [], [], []).

tree_to_ast(file, Name) ->
    Tree = get_tree(file, Name),
    tree_to_ast(Tree).

% Publics contain the list of functions to export
% Ast contains the ast it's final form
% CurrAttrs contains all the attributes found previous to a function
% Attrs contains the attributes modified to have the function they apply to

% all attributes are gattered until a function is found in CurrAttrs, then all
% are modified to contain the function/arity the apply to
tree_to_ast([], Publics, Ast, CurrAttrs, Attrs) ->
    {lists:reverse(Publics), lists:reverse(Ast),
        lists:reverse(lists:flatten(Attrs)) ++ lists:reverse(CurrAttrs)};

% global attributes are not tied to one function
tree_to_ast([{global_attribute, Line, Name, Value}|Tree], Publics, Ast, CurrAttrs, Attrs) ->
    tree_to_ast(Tree, Publics, Ast, CurrAttrs, [{attribute, Line, Name, Value}|Attrs]);

tree_to_ast([{attribute, _Line, _Name, _Value}=Attr|Tree], Publics, Ast, CurrAttrs, Attrs) ->
    tree_to_ast(Tree, Publics, Ast, [Attr|CurrAttrs], Attrs);

tree_to_ast([{public_function, Line, Name, Arity, Body}|Tree], Publics, Ast, CurrAttrs, Attrs) ->
    Fun = {Name, Arity},
    NewAttrs = modify_attrs(CurrAttrs, Fun),
    tree_to_ast(Tree, [Fun|Publics],
        [{function, Line, Name, Arity, Body}|Ast], [], [NewAttrs|Attrs]);

tree_to_ast([{function, _Line, Name, Arity, _Body}=H|Tree], Publics, Ast, CurrAttrs, Attrs) ->
    Fun = {Name, Arity},
    NewAttrs = modify_attrs(CurrAttrs, Fun),
    tree_to_ast(Tree, Publics,
        [H|Ast], [], [NewAttrs|Attrs]);
tree_to_ast([H|Tree], Publics, Ast, CurrAttrs, Attrs) ->
    tree_to_ast(Tree, Publics, [H|Ast], CurrAttrs, Attrs).

modify_attrs(Attrs, Fun) ->
    modify_attrs(Attrs, Fun, []).

modify_attrs([], _Fun, Accum) ->
    lists:reverse(Accum);
modify_attrs([{attribute, Line, Name, Args}|Attrs], Fun, Accum) ->
        modify_attrs(Attrs, Fun, [{attribute, Line, Name, {Fun, Args}}|Accum]).


get_publics(Tree) ->
    {Publics, _Ast, _Attrs} = tree_to_ast(Tree),
    Publics.

get_publics(From, String) ->
    Tree = get_tree(From, String),
    get_publics(Tree).

get_attrs(Tree) ->
    {_Publics, _Ast, Attrs} = tree_to_ast(Tree),
    Attrs.

get_attrs(From, String) ->
    Tree = get_tree(From, String),
    get_attrs(Tree).



% ast functions

get_ast(From, String) ->
    Tree = get_tree(From, String),
    {_Publics, Ast} = tree_to_ast(Tree),
    Ast.

print_ast(From, String) ->
    io:format("~p~n", [get_ast(From, String)]).

erl_to_ast(String) ->
    Scanned = case erl_scan:string(String) of
        {ok, Return, _} -> Return;
        Errors -> throw(Errors)
    end,

    case erl_parse:parse_exprs(Scanned) of
        {ok, Parsed} -> Parsed;
        Errors1 -> throw(Errors1)
    end.

% to erlang functions

get_erlang(From, String) ->
    Ast = get_ast(From, String),
    erl_prettypr:format(erl_syntax:form_list(Ast)).

print_erlang(From, String) ->
    Str = get_erlang(From, String),
    io:format("~s~n", [Str]).

% from erlang functions

from_erlang(Path) ->
    case epp:parse_file(Path, [], []) of
        {ok, Tree} -> Tree;
        Error -> throw(Error)
    end.

print_from_erlang(Path) ->
    Ast = from_erlang(Path),
    io:format("~p~n", [Ast]).

% compile functions
get_code(Ast) ->
    case compile:forms(Ast) of
        {ok, _, Code} -> Code;
        {ok, _, Code, _} -> Code;
        {error, Errors, _Warnings} -> throw(Errors);
        Error -> throw(Error)
    end.

compile(Name, Dir) ->
    Module = get_code(build_module(Name)),
    Path = filename:join([Dir, get_module_beam_name(Name)]),

    Device = case file:open(Path, [binary, write]) of
        {ok, Return} -> Return;
        {error, _Reason} = Error -> throw(Error)
    end,

    file:write(Device, Module).

build_module(Name) ->
    {Publics, Ast, Attrs} = tree_to_ast(file, Name),

    [{attribute, 1, module, get_module_name(Name)}|
        [{attribute, 1, export, Publics}|Attrs]] ++ Ast.

print_module(Name) ->
    io:format("~p~n", [build_module(Name)]).

% utils

remove_quotes(String) ->
    lists:reverse(tl(lists:reverse(tl(String)))).

file_to_string(Path) ->
    Content = case file:read_file(Path) of
        {ok, Return} -> Return;
        {error, _Reason} = Error -> throw(Error)
    end,

    binary_to_list(Content).

get_module_name(String) ->
    File = filename:basename(String),
    ModuleNameStr = filename:rootname(File),
    list_to_atom(ModuleNameStr).

get_module_beam_name(String) ->
    File = filename:basename(String),
    ModuleNameStr = filename:rootname(File),
    string:concat(ModuleNameStr, ".beam").

% eval functions

eval(Expression, Lang) ->
    Bindings = erl_eval:new_bindings(),

    try
        Ast = get_ast(string, Expression ++ "\n"),
        {value, Result, _} = erl_eval:exprs(Ast, Bindings),
        if
            Lang == efene ->
                io:format(">>> ~s~n~p~n", [Expression, Result]);

            Lang == erlang ->
                io:format("1> ~s~n~p~n",
                    [erl_prettypr:format(erl_syntax:form_list(Ast)), Result])
        end
    catch _:Error ->
        io:format("~p~n", [Error])
    end.

% command line interface

run() -> run([]).

run(["shell"]) ->
    fn_shell:start();
run(["eval", Expr]) ->
    eval(remove_quotes(Expr), efene);
run(["erleval", Expr]) ->
    eval(remove_quotes(Expr), erlang);
run(["lex", File]) ->
    fn_errors:handle(fun () -> print_lex(file, File) end);
run(["tree", File]) ->
    fn_errors:handle(fun () -> print_tree(file, File) end);
run(["ast", File]) ->
    fn_errors:handle(fun () -> print_ast(file, File) end);
run(["erl", File]) ->
    fn_errors:handle(fun () -> print_erlang(file, File) end);
run(["fn", File]) ->
   fn_errors:handle(fun () ->  fn_pp:pretty_print(get_lex(file, File), true) end);
run(["ifn", File]) ->
    fn_errors:handle(fun () -> fn_pp:pretty_print(get_lex(file, File), false) end);
run(["mod", File]) ->
    fn_errors:handle(fun () -> print_module(File) end);
run(["erl2ast", File]) ->
    fn_errors:handle(fun () -> print_from_erlang(File) end);
run(["beam", File]) ->
    fn_errors:handle(fun () -> compile(File, ".") end);
run(["beam", File, Dir]) ->
    fn_errors:handle(fun () -> compile(File, Dir) end);
run(Opts) ->
    io:format("Invalid input to fn.erl: \"~p\"~n", [Opts]).
