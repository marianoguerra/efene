% New BSD License, part of efene, see LICENSE for details

Definitions.

% binary operators
BinOr       = (\||\^)
BinAnd      = \&
Shift       = (<<|>>)
BinNot      = ~

% bool operators
BoolAnd     = and
BoolOr      = or
BoolAndd    = andd
BoolOrr     = orr
BoolXor     = xor
BoolNot     = not
Bool        = (true|false)

% arithmetic operators
Add         = (\+|-)
Mul         = (\*|//|/|%)
Comp        = (<|<=|==|===|>=|>|!=|!==)
Assign      = =
ColonEq     = :=

% list operators
Concat      = (\+\+|--)

% numbers
Number      = [0-9]
Float       = [0-9]+\.[0-9]+([eE][-+]?[0-9]+)?

% delimiters and operators
Open        = \(
Close       = \)
OpenList    = \[
CloseList   = \]
OpenMap    = \{
CloseMap   = \}
Sep         = ,
Endls       = (\s|\t)*(\r?\n|;)((\s|\t)*(\r?\n))*
Whites      = \s+
Tabs        = \t+
SplitDef    = ::
Colon       = :
Dot         = \.
Arrow       = ->
ArrowEnd    = ->>
LArrow      = <-
LArrowEnd   = <<-
FatArrow    = =>
Send        = !
Hash        = #
At          = @

% string stuff
String      = "(\\\^.|\\.|[^\"])*"

% identifiers and atoms
Identifier  = [A-Z\_][a-zA-Z0-9\_]*
Atom        = ([a-z][a-zA-Z0-9\_@]*)|('(\\\^.|\\.|[^\'])*')

Rules.

% binary operators
{Shift}                  : make_token(bin_shift, TokenLine, TokenChars).
{BinNot}                 : make_token(bin_not,  TokenLine, TokenChars).
{BinAnd}                 : make_token(bin_and,   TokenLine, TokenChars).
{BinOr}                  : make_token(bin_or,    TokenLine, TokenChars).

% bool operators
{BoolNot}                : make_token(bool_not,     TokenLine, TokenChars).
{Bool}                   : make_token(boolean,      TokenLine, TokenChars).
{BoolAnd}                : make_token(bool_and, TokenLine, TokenChars).
{BoolOr}                 : make_token(bool_or,  TokenLine, TokenChars).
{BoolAndd}               : make_token(bool_andd, TokenLine, TokenChars).
{BoolOrr}                : make_token(bool_orr,  TokenLine, TokenChars).
{BoolXor}                : make_token(bool_xor,  TokenLine, TokenChars).

% arithmetic operators
{Add}                    : make_token(add_op,  TokenLine, TokenChars).
{Mul}                    : make_token(mul_op,  TokenLine, TokenChars).
{Assign}                 : make_token(assign,   TokenLine, TokenChars).

{ColonEq}                : make_token(coloneq,   TokenLine, TokenChars).

{Comp}                   : make_token(comp_op, TokenLine, TokenChars).

% list operators
{Concat}                 : make_token(concat_op, TokenLine, TokenChars).

% numbers
{Float}                  : make_token(float,   TokenLine, TokenChars, fun erlang:list_to_float/1).
{Number}+                : make_token(integer, TokenLine, TokenChars, fun erlang:list_to_integer/1).

% delimiters and operators
{Open}                   : make_token(open,        TokenLine, TokenChars).
{Close}                  : make_token(close,       TokenLine, TokenChars).
{OpenList}               : make_token(open_list,   TokenLine, TokenChars).
{CloseList}              : make_token(close_list,  TokenLine, TokenChars).
{OpenMap}                : make_token(open_map,    TokenLine, TokenChars).
{CloseMap}               : make_token(close_map ,  TokenLine, TokenChars).

{Sep}                    : make_token(sep,          TokenLine, TokenChars).
{Send}                   : make_token(send_op,      TokenLine, TokenChars).
{Hash}                   : make_token(hash,         TokenLine, TokenChars).
{At}                     : make_token(at,         TokenLine, TokenChars).
{SplitDef}               : make_token(split_def_op, TokenLine, TokenChars).
{Colon}                  : make_token(colon,     TokenLine, TokenChars).
{Dot}                    : make_token(dot,          TokenLine, TokenChars).
{Arrow}                  : make_token(arrow,        TokenLine, TokenChars).
{ArrowEnd}               : make_token(arrowend,     TokenLine, TokenChars).
{LArrow}                 : make_token(larrow,       TokenLine, TokenChars).
{LArrowEnd}              : make_token(larrowend,       TokenLine, TokenChars).
{FatArrow}               : make_token(fatarrow,     TokenLine, TokenChars).

% string stuff
{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).

% identifiers and atoms
{Identifier}             : make_token(var, TokenLine, TokenChars).
{Atom}                   : {token, atom_or_identifier(TokenChars, TokenLine)}.

% spaces, tabs and new lines
{Endls}                 : make_token(nl, TokenLine, endls(TokenChars)).
{Whites}                : skip_token.
{Tabs}                  : skip_token.

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
is_reserved("do")      -> true;
is_reserved("for")     -> true;
is_reserved("in")      -> true;
is_reserved("begin")   -> true;
is_reserved("end")     -> true;
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

build_atom(Atom, _Line) -> list_to_atom(Atom).
