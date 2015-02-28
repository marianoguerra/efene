% sequence
-define(S(Line, Type, Val), {seq, Line, Type, Val}).
% val
-define(V(Line, Type, Val), {val, Line, Type, Val}).
% expression
-define(E(Line, Type, Val), {expr, Line, Type, Val}).
% operation
-define(O(Line, Type, Left, Right), {op, Line, Type, Left, Right}).
% unary operation
-define(UO(Line, Type, Val), {unary_op, Line, Type, Val}).
% tag
-define(T(Line, Tag, Val), {tag, Line, Tag, Val}).

-define(Atom(Val), ?V(_, atom, Val)).
-define(Var(Val), ?V(_, var, Val)).
-define(Int(Val), ?V(_, integer, Val)).
-define(Int(Line, Val), ?V(Line, integer, Val)).

-define(Type(Line), [?V(Line, atom, type)]).
-define(Spec(Line), [?V(Line, atom, spec)]).


