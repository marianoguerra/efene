-module(fn_gen).
-export([literal_to_ast/2, function/4]).

function(Line, Name, Args, Body) ->
    {function, Line, Name, length(Args),
        [{clause, Line, Args, [], Body}]}.

literal_to_ast(Val, Line) when is_tuple(Val) ->
    {tuple, Line,
        [literal_to_ast(V, Line) || V <- tuple_to_list(Val)]};
literal_to_ast(Val, Line) when is_boolean(Val) ->
    {boolean, Line, Val};
literal_to_ast(Val, Line) when is_integer(Val) ->
    {integer, Line, Val};
literal_to_ast(Val, Line) when is_float(Val) ->
    {float, Line, Val};
literal_to_ast(Val, Line) when is_atom(Val) ->
    {atom, Line, Val};
literal_to_ast([], Line) ->
    {nil, Line};
literal_to_ast([H|T], Line) ->
    {cons, Line, literal_to_ast(H, Line), literal_to_ast(T, Line)}.
