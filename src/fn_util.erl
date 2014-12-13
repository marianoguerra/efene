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
