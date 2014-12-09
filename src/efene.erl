-module(efene).
-export([run/0, run/1]).

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, binary_to_list(Content)};
        Other -> Other
    end.

with_file_content(Path, Fn) ->
    case read_file(Path) of
        {ok, Content} -> Fn(Content);
        Other -> Other
    end.

to_raw_lex(Path) -> with_file_content(Path, fun str_to_raw_lex/1).
to_lex(Path)     -> with_file_content(Path, fun str_to_lex/1).
to_ast(Path)     -> with_file_content(Path, fun str_to_ast/1).
to_erl_ast(Path) -> with_file_content(Path, fun str_to_erl_ast/1).

to_mod(Path) ->
    case to_erl_ast(Path) of
        {ok, Ast} ->
            ModAtomName = get_module_name(Path),
            ModAttr = {attribute, 1, module, ModAtomName},
            FileAttr = {attribute, 1, file, {Path, 1}},
            {ok, [FileAttr, ModAttr|Ast]};
        Other -> Other
    end.

to_erl(Path) ->
    case to_mod(Path) of
        {ok, Mod} -> erl_prettypr:format(erl_syntax:form_list(Mod));
        Other -> Other
    end.

to_code(Path) ->
    case to_mod(Path) of
        {ok, Ast} ->
            case compile:forms(Ast) of
                {ok, ModuleName, Code} -> {ok, ModuleName, Code, []};
                {ok, _ModuleName, _Code, _Warnings}=Res -> Res;
                Error -> Error
            end;
        Other -> Other
    end.

compile(Path) ->
    case to_code(Path) of
        {ok, _ModuleName, Code, Warnings} ->
            print(Warnings),
            BeamPath = get_module_beam_name(Path),
            bin_to_file(Code, BeamPath);
        Other -> Other
    end.

from_erl(Path) -> epp:parse_file(Path, [], []).

str_to_raw_lex(String) -> fn_lexer:string(String).

str_to_lex(String) ->
    case fn_lexer:string(String) of
        {ok, Tokens, NewLine} ->
            CleanTokens = clean_tokens(Tokens),
            {ok, CleanTokens, NewLine};
        Other -> Other
    end.

str_to_ast(Str) ->
    case str_to_lex(Str) of
        {ok, Tokens, _NewLine} -> fn_parser:parse(Tokens);
        Other -> Other
    end.

str_to_erl_ast(String) ->
    case str_to_ast(String) of
        {ok, Ast} -> {ok, fn_to_erl:ast_to_ast(Ast)};
        Other -> Other
    end.

% cli utilities

print({ok, Data}) ->
    print(Data);
print({error, {Line, fn_parser, Reason}}) ->
    io:format("error:~p: parse error: '~s'~n", [Line, Reason]);
print({error, {Line, fn_lexer, {illegal, Reason}}, _}) ->
    io:format("error:~p: illegal char ~p~n", [Line, Reason]);
print(Data) ->
    try io:format("~s~n", [Data]) catch
        _:_ -> io:format("~p~n", [Data])
    end.

% command line interface

run() -> run([]).

run(["rawlex", File]) ->
    print(to_raw_lex(File));
run(["lex", File]) ->
    print(to_lex(File));
run(["ast", File]) ->
    print(to_ast(File));
run(["mod", File]) ->
    print(to_mod(File));
run(["erlast", File]) ->
    print(to_erl_ast(File));
run(["erl", File]) ->
    print(to_erl(File));
run(["erl2ast", File]) ->
    print(from_erl(File));
run(["beam", File]) ->
    print(compile(File));
run(Opts) ->
    io:format("Invalid input to fn.erl: \"~p\"~n", [Opts]).

% private

clean_tokens(Tokens) -> clean_tokens(Tokens, []).

clean_tokens([], Accum) -> lists:reverse(Accum);
% remove newline after colon
clean_tokens([{colon, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after comma
clean_tokens([{sep, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after logical ops
clean_tokens([{bool_or, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bool_orr, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bool_xor, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bool_and, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bool_andd, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after ops
clean_tokens([{add_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{mul_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{comp_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{concat_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{send_op, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bin_shift, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bin_and, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{bin_or, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{assign, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{coloneq, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after open [ ( and {
clean_tokens([{open, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{open_list, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{open_map, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after arrows
clean_tokens([{arrow, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{arrowend, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{larrow, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{larrowend, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline after try, catch, after, receive, begin
clean_tokens([{'try', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'catch', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'after', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'receive', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'begin', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{'fn', _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);

% remove newline before end
clean_tokens([{nl, _, _}, {'end', _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline before closing ] ) and }
clean_tokens([{nl, _, _}, {close, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {close_list, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {close_map, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
% remove newline before arrows
clean_tokens([{nl, _, _}, {arrow, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {arrowend, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {larrow, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{nl, _, _}, {larrowend, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
% remove last endline
clean_tokens([{nl, _, _}], Accum) -> clean_tokens([], Accum);
clean_tokens([H|T], Accum) -> clean_tokens(T, [H|Accum]).

get_module_str_name(Path) ->
    BaseName = filename:basename(Path),
    filename:rootname(BaseName).

get_module_name(Path) ->
    list_to_atom(get_module_str_name(Path)).

get_module_beam_name(Path) ->
    ModuleNameStr = get_module_str_name(Path),
    string:concat(ModuleNameStr, ".beam").

bin_to_file(Bin, Path) ->
    to_file(Bin, Path, [binary, write]).

to_file(Data, Path, Mode) ->
    case file:open(Path, Mode) of
        {ok, Device} ->
            file:write(Device, Data),
            file:close(Device),
            ok;
        Error -> Error
    end.
