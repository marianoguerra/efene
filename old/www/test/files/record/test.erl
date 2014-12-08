-module(test).
-export([]).
-record(one, {one}).
-record(one_type, {one :: integer()}).
-record(one_type_default, {one=42 :: integer()}).

-record(two, {one, two}).
-record(two_type, {one :: integer(), two :: string() | binary()}).
-record(two_type_default, {one=42 :: integer(), two="foo" :: string() | binary()}).

-record(mixed, {one, two="foo", tres :: integer(), vier=4 :: integer(), fuenf="asd" :: string() | binary()}).
