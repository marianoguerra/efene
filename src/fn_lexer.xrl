% New BSD License, part of efene, see LICENSE for details

Definitions.

% binary operators
BinOr       = (\||\^)
BinAnd      = \&
Shift       = (<<|>>)
BinNot      = ~

% bool operators
BoolAndAlso = and
BoolAnd     = andd
BoolOr      = (orr|xor)
BoolOrElse  = or
BoolNot     = not
Bool        = (true|false)

% arithmetic operators
Add         = (\+|-)
Mul         = (\*|//|/|%)
Comp        = (<|<=|==|===|>=|>|!=|!==)
Match       = =

% list operators
Concat      = (\+\+|--)

% numbers
Number      = [0-9]
Float       = [0-9]+\.[0-9]+([eE][-+]?[0-9]+)?
BinNumber   = 0b[01]+
OctNumber   = 0o[0-7]+
HexNumber   = 0x[0-9a-fA-F]+

% delimiters and operators
OpenMetaBlock    = \$\(
OpenMetaOxford   = \$\[
Open        = \(
Close       = \)
OpenBlock   = {
CloseBlock  = }
OpenList    = \[
CloseList   = \]
OpenOxford  = \[\|
CloseOxford = \|\]
OpenBin     = <\[
CloseBin    = \]>
Sep         = ,
Endls       = (\s|\t|#(.*))*(\r?\n|;)((\s|\t|#(.*))*(\r?\n|;))*
Whites      = \s+
Tabs        = \t+
SplitDef    = ::
Split       = :
Dot         = \.
Arrow       = ->
LArrow       = <-
FatArrow    = =>
Send        = !

% string stuff
String      = "(\\\^.|\\.|[^\"])*"
Char        = \$\\?.
MacroVar    = \$[a-z][a-zA-Z0-9\_]+

% identifiers and atoms
Identifier  = [A-Z\_][a-zA-Z0-9\_]*
Atom        = ([a-z][a-zA-Z0-9\_@]*)|('(\\\^.|\\.|[^\'])*')

Rules.

% binary operators
{Shift}                  : make_token(shift_op, TokenLine, TokenChars).
{BinNot}                 : make_token(bin_not,  TokenLine, TokenChars).
{BinAnd}                 : make_token(and_op,   TokenLine, TokenChars).
{BinOr}                  : make_token(or_op,    TokenLine, TokenChars).

% bool operators
{BoolNot}                : make_token(bool_not,     TokenLine, TokenChars).
{Bool}                   : make_token(boolean,      TokenLine, TokenChars).
{BoolAndAlso}            : make_token(bool_andalso_op,  TokenLine, TokenChars).
{BoolAnd}                : make_token(bool_and_op, TokenLine, TokenChars).
{BoolOrElse}             : make_token(bool_orelse_op,   TokenLine, TokenChars).
{BoolOr}                 : make_token(bool_or_op,  TokenLine, TokenChars).

% arithmetic operators
{Add}                    : make_token(add_op,  TokenLine, TokenChars).
{Mul}                    : make_token(mul_op,  TokenLine, TokenChars).
{Match}                  : make_token(match,   TokenLine, TokenChars).

{Comp}                   : make_token(comp_op, TokenLine, TokenChars).

% list operators
{Concat}                 : make_token(concat_op, TokenLine, TokenChars).

% numbers
{Float}                  : make_token(float,   TokenLine, TokenChars, fun erlang:list_to_float/1).
{Number}+                : make_token(integer, TokenLine, TokenChars, fun erlang:list_to_integer/1).
{BinNumber}              : make_token(integer, TokenLine, TokenChars, fun bin_to_integer/1).
{OctNumber}              : make_token(integer, TokenLine, TokenChars, fun oct_to_integer/1).
{HexNumber}              : make_token(integer, TokenLine, TokenChars, fun hex_to_integer/1).

% delimiters and operators
{OpenMetaBlock}          : make_token(open_meta_block,  TokenLine, TokenChars).
{OpenMetaOxford}         : make_token(open_meta_oxford, TokenLine, TokenChars).
{OpenBin}                : make_token(open_bin,    TokenLine, TokenChars).
{CloseBin}               : make_token(close_bin,   TokenLine, TokenChars).
{Open}                   : make_token(open,        TokenLine, TokenChars).
{Close}                  : make_token(close,       TokenLine, TokenChars).
{OpenBlock}              : make_token(open_block,  TokenLine, TokenChars).
{CloseBlock}             : make_token(close_block, TokenLine, TokenChars).
{OpenList}               : make_token(open_list,   TokenLine, TokenChars).
{CloseList}              : make_token(close_list,  TokenLine, TokenChars).
{OpenOxford}             : make_token(open_oxford, TokenLine, TokenChars).
{CloseOxford}            : make_token(close_oxford,TokenLine, TokenChars).

{Sep}                    : make_token(sep,          TokenLine, TokenChars).
{Send}                   : make_token(send_op,      TokenLine, TokenChars).
{SplitDef}               : make_token(split_def_op, TokenLine, TokenChars).
{Split}                  : make_token(split_op,     TokenLine, TokenChars).
{Dot}{Dot}{Dot}          : make_token(dotdotdot,    TokenLine, TokenChars).
{Dot}{Dot}               : make_token(dotdot,       TokenLine, TokenChars).
{Dot}                    : make_token(dot,          TokenLine, TokenChars).
{Arrow}                  : make_token(arrow,        TokenLine, TokenChars).
{LArrow}                 : make_token(larrow,       TokenLine, TokenChars).
{FatArrow}               : make_token(fatarrow,     TokenLine, TokenChars).

% string stuff
{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).
{MacroVar}               : make_token(macrovar, TokenLine, tl(TokenChars)).
{Char}                   : make_token(char,     TokenLine, TokenChars, fun(Char) -> list_to_charnum(Char, TokenLine) end).

% identifiers and atoms
{Identifier}             : make_token(var, TokenLine, TokenChars).
@{Identifier}            : make_token(obj, TokenLine, tl(TokenChars)).
{Atom}                   : {token, atom_or_identifier(TokenChars, TokenLine)}.
@@{Atom}                 : make_token(gattr, TokenLine, tl(tl(TokenChars))).
@{Atom}                  : make_token(attr, TokenLine, tl(TokenChars)).

% spaces, tabs and new lines
{Endls}                 : make_token(endl,  TokenLine, endls(TokenChars)).
{Whites}                : make_token(white, TokenLine, length(TokenChars)).
{Tabs}                  : make_token(tab,   TokenLine, length(TokenChars)).

Erlang code.

make_token(Name, Line, Chars) when is_list(Chars) ->
    {token, {Name, Line, list_to_atom(Chars)}};
make_token(Name, Line, Chars) ->
    {token, {Name, Line, Chars}}.

make_token(Name, Line, Chars, Fun) ->
    {token, {Name, Line, Fun(Chars)}}.

endls(Chars) ->
    lists:filter(fun (C) -> C == $\n orelse C == $; end, Chars).

atom_or_identifier(String, TokenLine) ->
     case is_reserved(String) of
         true ->
            {list_to_atom(String), TokenLine};
         false ->
            {atom, TokenLine, build_atom(String, TokenLine)}
     end.

bin_to_integer("0b" ++ Number) ->
    {ok, [Val], _} = io_lib:fread("~2u", Number),
    Val.

oct_to_integer("0o" ++ Number) ->
    {ok, [Val], _} = io_lib:fread("~8u", Number),
    Val.

hex_to_integer("0x" ++ Number) ->
    {ok, [Val], _} = io_lib:fread("~16u", Number),
    Val.

is_reserved("if")      -> true;
is_reserved("else")    -> true;
is_reserved("try")     -> true;
is_reserved("catch")   -> true;
is_reserved("receive") -> true;
is_reserved("after")   -> true;
is_reserved("case")    -> true;
is_reserved("switch")  -> true;
is_reserved("when")    -> true;
is_reserved("fn")      -> true;
is_reserved("for")     -> true;
is_reserved("in")      -> true;
is_reserved("begin")   -> true;
is_reserved(_)         -> false.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2), Line),
    {token, {Type, Line, String}}.

unescape_string(String, Line) -> unescape_string(String, Line, []).

unescape_string([], _Line, Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Line, Output) ->
  Char = map_escaped_char(Escaped, Line),
  unescape_string(Rest, Line, [Char|Output]);
unescape_string([Char|Rest], Line, Output) ->
  unescape_string(Rest, Line, [Char|Output]).

map_escaped_char(Escaped, Line) ->
  case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $\( -> $(;
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {Line, fn_lexer, ["unrecognized escape sequence: ", [$\\, Escaped]]}})
  end.

list_to_charnum([_, $\\, Char], Line) ->
  map_escaped_char(Char, Line);
list_to_charnum([_, Char], _Line) -> Char.

build_atom([$'|_] = Atom, Line) ->
    list_to_atom(unescape_string(lists:sublist(Atom, 2, length(Atom) - 2), Line));
build_atom(Atom, _Line) -> list_to_atom(Atom).
