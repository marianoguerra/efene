-module(params).
-export([]).

-type orddict(Key, Val) :: [{Key, Val}].

% some types taken from erl_scan.erl
-type category() :: atom().
-type column() :: pos_integer().
-type line() :: integer().
-type nonemptyintlist() :: [int(),...].
-type location() :: line() | {line(),column()}.
-type resword_fun() :: fun((atom()) -> boolean()).
-type resword_fun1() :: fun((atom()) -> boolean() | integer()).
-type resword_fun2() :: fun((atom()) -> 'atom' | 'another').
-type resword_fun3() :: fun((atom()) -> 42 | 'foo').
-type resword_fun4() :: fun((...) -> 42 | 'foo').
-type option() :: 'return' | 'return_white_spaces' | 'return_comments' | 'text' | {'reserved_word_fun', resword_fun()}.
-type options() :: option() | [option()].
-type symbol() :: atom() | float() | integer() | string().
-type info_line() :: integer() | term().
-type attributes_data()
       :: [{'column', column()} | {'line', info_line()} | {'text', string()}]
        | {line(), column()}.
%% The fact that {line(),column()} is a possible attributes() type
%% is hidden.
-type attributes() :: line() | attributes_data().
-type token() :: {category(), attributes(), symbol()}
               | {category(), attributes()}.
-type tokens() :: [token()].
-type error_description() :: term().
-type error_info() :: {location(), module(), error_description()}.
