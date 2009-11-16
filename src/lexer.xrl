Definitions.

Or            = | 
Identifier    = [A-Z_][a-zA-Z0-9_]*
Atom          = [a-z][a-zA-Z0-9_@]*
Uppercase     = [A-Z]
Number        = [0-9]
BinNumber     = 0b[01]+
OctNumber     = 0o[0-7]+
HexNumber     = 0x[0-9a-fA-F]+
ConcatOp      = (\+\+|--)
ShiftOp       = (<<|>>)
AddOp         = (\+|-)
MulOp         = (\*|/|%)
CompOp        = (<|<=|==|===|>=|>|!=|!==)
BoolAndOp     = and
BoolOrOp      = or
White         = (\s|\t|\\\n)
Comment       = #.*?
Open          = \(
Close         = \)
OpenBlock     = {
CloseBlock    = }
OpenList      = \[
CloseList     = \]
OpenBin       = <\[
CloseBin      = \]>
Match         = =
Bool          = (true|false)
Fn            = fn
Sep           = ,
End           = ((\s|\t)*\n)+
UnaryOp       = (not|~)
% the string stuff taken from Reia
String        = "(\\\^.|\\.|[^\"])*"
Split         = :
Dot           = \.

Rules.

{OpenBin}                : {token, {open_bin,    TokenLine, list_to_atom(TokenChars)}}.
{CloseBin}               : {token, {close_bin,   TokenLine, list_to_atom(TokenChars)}}.
{Or}                     : {token, {or_op,       TokenLine, list_to_atom(TokenChars)}}.
{ShiftOp}                : {token, {shift_op,    TokenLine, list_to_atom(TokenChars)}}.
{ConcatOp}               : {token, {concat_op,   TokenLine, list_to_atom(TokenChars)}}.
{AddOp}                  : {token, {add_op,      TokenLine, list_to_atom(TokenChars)}}.
{MulOp}                  : {token, {mul_op,      TokenLine, list_to_atom(TokenChars)}}.
{UnaryOp}                : {token, {unary_op,    TokenLine, list_to_atom(TokenChars)}}.
{Number}+                : {token, {integer,     TokenLine, list_to_integer(TokenChars)}}.
{Number}+\.{Number}+     : {token, {float,       TokenLine, list_to_float(TokenChars)}}.
{BinNumber}              : {token, {integer,     TokenLine, bin_to_integer(TokenChars)}}.
{OctNumber}              : {token, {integer,     TokenLine, oct_to_integer(TokenChars)}}.
{HexNumber}              : {token, {integer,     TokenLine, hex_to_integer(TokenChars)}}.
{Bool}                   : {token, {boolean,     TokenLine, list_to_atom(TokenChars)}}.
{BoolAndOp}              : {token, {bool_and_op, TokenLine, list_to_atom(TokenChars)}}.
{BoolOrOp}               : {token, {bool_or_op,  TokenLine, list_to_atom(TokenChars)}}.
{CompOp}                 : {token, {comp_op,     TokenLine, list_to_atom(TokenChars)}}.
{Open}                   : {token, {open,        TokenLine, list_to_atom(TokenChars)}}.
{Close}                  : {token, {close,       TokenLine, list_to_atom(TokenChars)}}.
{OpenBlock}{End}?        : {token, {open_block,  TokenLine, '{'}}.
{CloseBlock}             : {token, {close_block, TokenLine, list_to_atom(TokenChars)}}.
{OpenList}               : {token, {open_list,   TokenLine, list_to_atom(TokenChars)}}.
{CloseList}              : {token, {close_list,  TokenLine, list_to_atom(TokenChars)}}.
{Fn}                     : {token, {fn,          TokenLine, list_to_atom(TokenChars)}}.
{Sep}                    : {token, {sep,         TokenLine, list_to_atom(TokenChars)}}.
{End}                    : {token, {endl,        TokenLine, list_to_atom(TokenChars)}}.
{Match}                  : {token, {match,       TokenLine, list_to_atom(TokenChars)}}.
{Identifier}             : {token, {var,         TokenLine, list_to_atom(TokenChars)}}.
{Atom}                   : {token, atom_or_identifier(TokenChars, TokenLine)}.
&                        : {token, {and_op,      TokenLine, list_to_atom(TokenChars)}}.
!                        : {token, {send_op,     TokenLine, list_to_atom(TokenChars)}}.
\^                       : {token, {xor_op,      TokenLine, list_to_atom(TokenChars)}}.
{Split}                  : {token, {split_op,    TokenLine, list_to_atom(TokenChars)}}.
{Dot}                    : {token, {dot,    TokenLine, list_to_atom(TokenChars)}}.
{White}+                 : skip_token.
{Comment}{End}           : skip_token.
{String}                 : build_string(string, TokenChars, TokenLine, TokenLen).


Erlang code.

atom_or_identifier(String, TokenLine) ->
     case is_reserved(String) of
         true -> 
            {list_to_atom(String), TokenLine};
         false ->   
            {atom, TokenLine, list_to_atom(String)}
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

is_reserved("if") -> true;
is_reserved("try") -> true;
is_reserved("catch") -> true;
is_reserved("finally") -> true;
is_reserved("receive") -> true;
is_reserved("after") -> true;
is_reserved("object") -> true;
is_reserved(_) -> false.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2)),
    {token, {Type, Line, String}}.

unescape_string(String) -> unescape_string(String, []).
 
unescape_string([], Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Output) ->
  Char = case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {"unrecognized escape sequence: ", [$\\, Escaped]}})
  end,
  unescape_string(Rest, [Char|Output]);
unescape_string([Char|Rest], Output) ->
  unescape_string(Rest, [Char|Output]).
