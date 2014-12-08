-module(simple).
-export([]).
% simple type
-type type1() :: integer().
% union type
-type type2() :: integer() | list().
% union with custom type
-type type3() :: integer() | list() | type1().
% simple literal type
-type one() :: 1.
% union of literals
-type fals() :: 0 | false | [] | <<>> | {}.
