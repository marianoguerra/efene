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

-module(fn_util).
-export([get_exported_fns/1, get_export_attr_ast/1, get_export_attr_ast/2]).

-include("efene.hrl").

get_fn_attrs(#{attrs:=Attrs}) ->
    lists:filter(fun ({fn, _Line, _Fn, _FnAttrs}) -> true;
                     (_) -> false end, Attrs).


is_public_attr({attr, _, [?V(_, atom, public)], noparams, noresult}) -> true;
is_public_attr(_) -> false.

is_public_fn({fn, _Line, _Fn, Attrs}) ->
    lists:any(fun is_public_attr/1, Attrs);
is_public_fn(_) -> false.

get_exported_fns(State) ->
    OnlyFnAttrs =  get_fn_attrs(State),
    lists:filter(fun is_public_fn/1, OnlyFnAttrs).

get_export_attr_ast(State) ->
    get_export_attr_ast(State, 2).

get_export_attr_ast(State, Line) ->
    PublicFns = get_exported_fns(State),
    Exports = lists:map(fun ({fn, _Line, Fn, _Attrs}) -> Fn end, PublicFns),
    {attribute, Line, export, Exports}.
