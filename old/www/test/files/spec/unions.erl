-module(unions).
-export([f_1/1, f_2/1, f_3/1, f_4/1]).
% test simple union of default types
-spec f_1(integer() | float()) -> integer() | float().
f_1(Val) ->
    Val.

% test union of all default types
-spec f_2(any() | none() | pid() | port() | ref() | float() | integer() | atom() | list () | binary() | fun() | tuple()) ->
    any() | none() | pid() | port() | ref() | float() | integer() | atom() | list () | binary() | fun() | tuple().
f_2(Val) ->
    Val.

% test simple union of custom types
-spec f_3(foo() | bar()) -> foo() | bar().
f_3(Val) ->
    Val.

% test simple union of literals
-spec f_4(0 | false | [] | {} | <<>>) -> 0 | false | [] | {} | <<>>.
f_4(Val) ->
    Val.

