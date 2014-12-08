-module(fn_gen).
-compile(export_all).

-license("New BSD License, part of efene, see LICENSE for details").

literal_to_ast(Val, Line) ->
    literal_to_ast(Val, Line, false).

literal_to_ast(Val, Line, SpecialAtomToVar) when is_tuple(Val) ->
    {tuple, Line,
        [literal_to_ast(V, Line, SpecialAtomToVar) || V <- tuple_to_list(Val)]};
literal_to_ast(Val, Line, _SpecialAtomToVar) when is_boolean(Val) ->
    {atom, Line, Val};
literal_to_ast(Val, Line, _SpecialAtomToVar) when is_integer(Val) ->
    {integer, Line, Val};
literal_to_ast(Val, Line, _SpecialAtomToVar) when is_float(Val) ->
    {float, Line, Val};
literal_to_ast(Val, Line, SpecialAtomToVar) when is_atom(Val) ->
    if
        SpecialAtomToVar ->
            [FirstChar|VarName] = atom_to_list(Val),


            if
                FirstChar == $$ -> {var, Line, list_to_atom(VarName)};
                true -> {atom, Line, Val}
            end;

        true ->
            {atom, Line, Val}
    end;
literal_to_ast(Val, Line, SpecialAtomToVar) when is_list(Val) ->
    list_or_string_to_ast(Val, Line, SpecialAtomToVar).

list_or_string_to_ast(List, Line, SpecialAtomToVar) ->
    IsString = is_string(List),

    if
        IsString ->
            {string, Line, List};
        true ->
            list_to_ast(List, Line, SpecialAtomToVar)
    end.

list_to_ast([], Line, _SpecialAtomToVar) ->
    {nil, Line};
list_to_ast([H|T], Line, SpecialAtomToVar) ->
    {cons, Line, literal_to_ast(H, Line, SpecialAtomToVar), literal_to_ast(T, Line, SpecialAtomToVar)}.

is_string([]) ->
    false;
is_string(List) ->
    is_string1(List).

is_string1([]) ->
    true;
is_string1([H|T]) when is_integer(H), H >= 0 andalso H < 256 ->
    is_string1(T);
is_string1(_) ->
    false.

