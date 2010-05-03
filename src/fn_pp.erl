-module(fn_pp).
-export([pretty_print/2]).

% pretty print from tokens, the tokens must be in the final format
% that means with blocks and no whitespaces or tabs
pretty_print(Tokens, PrintBlocks) ->
    pretty_print(Tokens, PrintBlocks, 0).

pretty_print([], _PrintBlocks, _Indent) ->
    io:format("\n"),
    ok;

pretty_print([{open_block, _, Str}|Tokens], PrintBlocks, Indent) ->
    NewIndent = Indent + 4,

    if
        PrintBlocks ->
            print_token(Str);
        true ->
            ok
    end,

    io:format("\n"),
    io:format(lists:duplicate(NewIndent, $\s)),

    pretty_print(Tokens, PrintBlocks, NewIndent);

pretty_print([{close_block, _, Str}|Tokens], PrintBlocks, Indent) when Indent > 0 ->
    io:format(lists:duplicate(4, $\b)),
    if
        PrintBlocks ->
            print_token(Str);
        true ->
            ok
    end,

    pretty_print(Tokens, PrintBlocks, Indent - 4);

pretty_print([{Atom, _Line}|Tokens], PrintBlocks, Indent) ->
    print_token(Atom),
    pretty_print(Tokens, PrintBlocks, Indent);

pretty_print([{endl, _Line, Count}|Tokens], PrintBlocks, Indent) ->
    if
        PrintBlocks ->
            io:format(lists:duplicate(Count, $\n));
        true ->
            io:format("~n")
    end,

    io:format(lists:duplicate(Indent, $\s)),
    pretty_print(Tokens, PrintBlocks, Indent);

pretty_print([{_Token, _Line, Str}|Tokens], PrintBlocks, Indent) ->
    print_token(Str),
    pretty_print(Tokens, PrintBlocks, Indent).

print_token(Str) when is_atom(Str)->
    Token = atom_to_list(Str),

    ToPrint = case print_space(Str) of
        none ->
            Token;
        both ->
            " " ++ Token ++ " ";
        before ->
            " " ++ Token;
        'after' ->
            Token ++ " "
    end,
    io:format(ToPrint);

print_token(Str) when is_integer(Str)->
    io:format(integer_to_list(Str));
print_token(Str) when is_float(Str)->
    io:format(float_to_list(Str));
print_token(Str) when is_list(Str) ->
    io:format("\"~s\"", [Str]).

print_space('+') -> 'both';
print_space('-') -> 'both';
print_space('*') -> 'both';
print_space('/') -> 'both';
print_space('%') -> 'both';
print_space('&') -> 'both';
print_space('^') -> 'both';
print_space('when') -> 'both';
print_space('and') -> 'both';
print_space('andd') -> 'both';
print_space('or') -> 'both';
print_space('orr') -> 'both';
print_space('xor') -> 'both';
print_space('not') -> 'both';
print_space('true') -> 'both';
print_space('false') -> 'both';
print_space('<<') -> 'both';
print_space('>>') -> 'both';
print_space('==') -> 'both';
print_space('===') -> 'both';
print_space('!=') -> 'both';
print_space('!==') -> 'both';
print_space('>') -> 'both';
print_space('<') -> 'both';
print_space('<=') -> 'both';
print_space('>=') -> 'both';
print_space('++') -> 'both';
print_space('--') -> 'both';
print_space('<[') -> 'before';
print_space(',') -> 'after';
print_space('{') -> 'before';
print_space('}') -> 'after';
print_space('[') -> 'before';
print_space('=') -> 'both';
print_space('public') -> 'after';
print_space('if') -> 'after';
print_space('else') -> 'after';
print_space('try') -> 'after';
print_space('catch') -> 'after';
print_space('receive') -> 'after';
print_space('after') -> 'after';
print_space('case') -> 'after';
print_space('switch') -> 'after';
print_space('break') -> 'after';
print_space('object') -> 'after';
print_space('for') -> 'after';
print_space('in') -> 'after';
print_space('fn') -> 'after';
print_space(_) -> none.
