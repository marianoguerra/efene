%% Copyright 2015 Mariano Guerra
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(efene).
-export([run/0, run/1, compile/2, to_code/1,
         to_raw_lex/1, to_lex/1, to_ast/1, to_erl_ast/1, to_mod/1, pprint/1]).

read_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} -> {ok, unicode:characters_to_list(Content, utf8)};
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
to_erl_ast(Path) -> with_file_content(Path, fun (Str) ->
                                                Module = get_module_name(Path),
                                                str_to_erl_ast(Str, Module)
                                            end).

to_mod(Path) ->
    case to_erl_ast(Path) of
        {ok, {Ast, State}} ->
            ModAtomName = get_module_name(Path),
            ToMod = fun () ->
                            ExportAttr = fn_util:get_export_attr_ast(State),
                            FnAttrs = fn_util:get_fn_attribute(State),
                            ModAttr = {attribute, 1, module, ModAtomName},
                            FileAttr = {attribute, 1, file, {Path, 1}},
                            case ExportAttr of
                                {attribute, _ExportLine, export, []} ->
                                    {ok, [FileAttr, ModAttr|(FnAttrs ++ Ast)]};
                                _ ->
                                    {ok, [FileAttr, ModAttr, ExportAttr|(FnAttrs ++ Ast)]}
                            end
                    end,
            format_errors_or(ModAtomName, State, ToMod);
        Other -> Other
    end.

pprint(Path) ->
    case to_ast(Path) of
        {ok, Ast} -> io:format("~s", [fn_pp:print(Ast)]);
        Other -> io:format("Error: ~p", [Other])
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
    compile(Path, ".").

compile(Path, DestPath) ->
    case to_code(Path) of
        {ok, _ModuleName, Code, Warnings} ->
            print(Warnings),
            BeamPath = filename:join(DestPath, get_module_beam_name(Path)),
            bin_to_file(Code, BeamPath);
        Other -> Other
    end.

from_erl(Path) -> epp:parse_file(Path, [], []).

str_to_raw_lex(String) -> fn_lexer:string(String).

str_to_lex(String) ->
    case fn_lexer:string(String) of
        {ok, Tokens, Endline} ->
            CleanTokens = clean_tokens(Tokens),
            {ok, CleanTokens, Endline};
        {eof, Endline} -> {error, {Endline, fn_lexer, {eof, Endline}}};
        {error, Error} -> {error, Error}
    end.

str_to_ast(Str) ->
    case str_to_lex(Str) of
        {ok, Tokens, _NewLine} -> fn_parser:parse(Tokens);
        Other -> Other
    end.

str_to_erl_ast(String, Module) ->
    case str_to_ast(String) of
        {ok, Ast} -> {ok, fn_to_erl:to_erl(Ast, Module)};
        Other -> Other
    end.

% cli utilities

print({ok, Data}) ->
    print(Data);
print({error, _}=Error) ->
    Reason = fn_error:normalize(Error),
    io:format("error:~s~n", [Reason]);
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
run(["pprint", File]) ->
    pprint(File);
run(["testpp"]) ->
    test_pp();
run(Opts) ->
    io:format("Invalid input to fn.erl: \"~p\"~n", [Opts]).

% private

clean_tokens(Tokens) -> clean_tokens(Tokens, []).

clean_tokens([], Accum) -> lists:reverse(Accum);
% remove newline after colon, comma and semicolon
clean_tokens([{colon, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
clean_tokens([{semicolon, _, _}=H, {nl, _, _}|T], Accum) -> clean_tokens([H|T], Accum);
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
clean_tokens([{'fn', _}=H1, {var, _, _}=H2, {nl, _, _}|T], Accum) -> clean_tokens([H1, H2|T], Accum);
clean_tokens([{'fn', _}=H1, {atom, _, _}=H2, {nl, _, _}|T], Accum) -> clean_tokens([H1, H2|T], Accum);

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
% remove duplicated endlines
clean_tokens([{nl, _, _}, {nl, _, _}=H|T], Accum) -> clean_tokens([H|T], Accum);
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

format_errors_or(_Module, #{errors:=[]}, Fn) -> Fn();
format_errors_or(Module, #{errors:=Errors}, _Fn) ->
    ErrorsFirstToLast = lists:reverse(Errors),
    {error, {efene, Module, fn_error:to_string(Module, ErrorsFirstToLast)}}.

pp(Code) ->
    case str_to_ast(Code) of
        {ok, Ast} -> io:format("~s => ~s", [Code, fn_pp:print(Ast)]);
        Other -> io:format("Error: ~s -> ~p~n", [Code, Other])
    end.
    
test_pp() ->
    pp("1"),
    pp("1.2"),
    pp("true"),
    pp("atom"),
    pp("Var"),
    pp("nil"),

    pp("'\"binary\" \\'string\\''"),
    pp("\"'list' \\\"string\\\"\""),

    pp("{}"),
    pp("[]"),
    pp("#{}"),
    pp("{a: 1}"),
    pp("{a: 1, b: true}"),
    pp("{a: 1, b: true, c: asd}"),
    pp("{a:= 1}"),
    pp("{a:= 1, b:= true}"),
    pp("{a:= 1, b:= true, c:= asd}"),
    pp("[1]"),
    pp("[1::2]"),
    pp("[1::[1]]"),
    pp("[1, 1.2]"),
    pp("[1, 1.2, nil]"),
    pp("[1, 1.2, nil, {a, V}]"),
    pp("{1}"),
    pp("{1,}"),
    pp("{1, 1.2}"),
    pp("{1, 1.2,}"),
    pp("{1, 1.2, nil}"),
    pp("{1, 1.2, nil, {a, V}}"),

    pp("#foo 42"),

    pp("(-2)"),
    pp("(+2)"),
    pp("(~2)"),
    pp("(-2 + +3)"),
    pp("(+2 - -3)"),
    pp("(-2 + -3)"),
    pp("(+2 - +3)"),
    pp("(~2 | 3)"),
    pp("(not true)"),
    pp("(a ! 2)"),
    pp("(a ! 2)"),
    pp("(1 = 2)"),
    pp("(1 + 2)"),
    pp("(1 * 2)"),
    pp("(1 + 2 * 3)"),
    pp("(1 * 2 + 3)"),
    pp("(1 * (2 + 3))"),
    pp("((1 + 2) * 3)"),
    pp("(3 - 5 - 7)"),
    pp("((3 - 5) - 7)"),
    pp("(3 - (5 - 7))"),
    pp("(1 < 2 and 2 >= 3 or 4 + 5)"),
    pp("(1 < (2 and 2) >= (3 or 4) + 5)"),
    pp("(true and false or nil)"),
    pp("(true or false and nil)"),

    pp("@public"),
    pp("@public(1)"),
    pp("@public(1, asd)"),
    pp("@public(1) -> 2"),
    pp("@public -> 2"),
    pp("@public(1, true) -> 2"),
    pp("@public(1) -> {2, 3}"),
    pp("@public(1, true) -> [2]"),
    pp("fn:foo:4"),
    pp("fn:foo.bar:4"),
    pp("foo(a, B, 4, [1,2,3])"),
    pp("foo.bar(a, B, 4, [1,2,3])"),

    pp("(begin 42\nend)"),
    pp("(begin A = 42\nA + B\nend)"),
    pp("(begin A = 42\nbegin B = 12\nA + B\nend\nend)"),
    pp("(begin A = 42\nbegin B = 12\nbegin C = 1\nC + A + B\nend\nend\nend)"),

    pp("(receive case A: B\nend)"),
    pp("(receive case A: B\nelse: error\nend)"),
    pp("(receive case A: B\ncase A, B: C = A + B\nC * 2\nelse: error\nend)"),
    pp("(receive case A: B\nafter 5: ahhhhh\nend)"),
    pp("(receive case A: B\ncase A, B: C = A + B\nC * 2\nelse: error\nafter 10:\nD = 10\nE = foo(12)\nD * 2\nend)"),
    pp("(receive case A when A < 10: B\nend)"),
    pp("(receive case A when A < 10, A > 20; A == 12, A == 13: B\nend)"),

    pp("(match A: case 42: one\ntwo\nend)"),
    pp("(match A: case 42: one\ntwo\ncase 43: cuatro\nend)"),
    pp("(match A: case 42: one\ntwo\ncase 43: cuatro\nelse: ahh\nend)"),
    pp("(match A: case 42 when A < 10; A == 11, A == 12; A == 13, A == 14, A == 15: one\ntwo\nend)"),

    pp("(try 42\nafter finalthing\nend)"),
    pp("(try 42\nA + B\nafter finalthing\nend)"),
    pp("(try 42\nA + B\ncatch case E: error\nafter finalthing\nend)"),
    pp("(try 42\nA + B\ncatch case T: throw\ncase error,E: error\nafter finalthing\nend)"),

    pp("(when true: ok\nend)"),
    pp("(when true: ok\nelse 42: two\nend)"),
    pp("(when true: ok\nelse 42: two\nelse A < B, A == 12; A > 13: A = 12\nB = A + 5\nend)"),
    pp("(when true: ok\nelse 42: two\nelse A < B, A == 12; A > 13: A = 12\nB = A + 5\nelse: default\nend)"),

    pp("fn hello case A: A + 1\nend"),
    pp("fn hello @public\n@doc('hi there')\n@spec(A) -> bool()\ncase A: A + 1\nend"),
    pp("fn hello case A: A + 1\nelse: 42\nend"),
    pp("(fn case A: A + 1\nend)"),
    pp("(fn F case A: A + 1\nend)"),

    pp("(A -> foo())"),
    pp("(A -> foo() ->> bar(1))"),
    pp("(A -> foo() ->> bar(1) -> a.b([]))"),
    pp("(A -> foo() ->> bar(1) -> a.b([]) ->> A.B(1, 2.3))"),

    pp("(for A in B: B + 1\nend)"),
    pp("(for A in B; A < 10: B + 1\nend)"),
    ok.
