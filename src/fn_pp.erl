-module(fn_pp).
-export([print/1]).

-include("efene.hrl").

ind(0) -> "";
ind(1) -> "  ";
ind(2) -> "    ";
ind(3) -> "      ";
ind(4) -> "        ";
ind(Indent) -> string:copies("  ", Indent).

fmt(Str, Fmt, true, Indent, Args) -> [Str, ind(Indent), io_lib:format(Fmt, Args), "\n"];
fmt(Str, Fmt, false, Indent, Args) -> [Str, ind(Indent), io_lib:format(Fmt, Args)].

print(Nodes) -> print(Nodes, "", true, 0, [], "\n").
%print(Nodes, Indent) -> print(Nodes, "", true, Indent, [], "\n").
print_single(Node) -> print(Node, "", false, 0).

print(Nodes, Str, Nl, Indent) when is_list(Nodes) -> print(Nodes, Str, Nl, Indent, [], "");

print({attr, _L, Path, noparams, noresult}, Str, Nl, Indent) ->
    fmt(Str, "@~s", Nl, Indent, [print_path(Path)]);
print({attr, _L, Path, Params, noresult}, Str, Nl, Indent) ->
    fmt(Str, "@~s(~s)", Nl, Indent, [print_path(Path), print_seq(Params)]);
print({attr, _L, Path, noparams, Result}, Str, Nl, Indent) ->
    fmt(Str, "@~s -> ~s", Nl, Indent, [print_path(Path), print_single(Result)]);
print({attr, _L, Path, Params, Result}, Str, Nl, Indent) ->
    fmt(Str, "@~s(~s) -> ~s", Nl, Indent, [print_path(Path), print_seq(Params), print_single(Result)]);

print(?E(_L, call_do, {first, Call, Fun}), Str, Nl, Indent) ->
    fmt(Str, "~s <- ~s", Nl, Indent, [print(Call), print(Fun)]);
print(?E(_L, call_do, {last, Call, Fun}), Str, Nl, Indent) ->
    fmt(Str, "~s <<- ~s", Nl, Indent, [print(Call), print(Fun)]);

print(?E(_L, call_thread, {InitialVal, Calls}), Str, Nl, Indent) ->
    fmt(Str, "~s~n~s", Nl, Indent,
        [print_single(InitialVal), print_thread_calls(Calls, Indent + 1)]);

print(?S(_L, list, Val), Str, Nl, Indent) ->
    fmt(Str, "[~s]", Nl, Indent, [print_seq(Val)]);

print(?S(_L, map, {Var, KVs}), Str, Nl, Indent) ->
    fmt(Str, "~s#{~s}", Nl, Indent, [print_single(Var), print_kvs(KVs)]);
print(?S(_L, map, KVs), Str, Nl, Indent) ->
    fmt(Str, "{~s}", Nl, Indent, [print_kvs(KVs)]);

print(?T(_L, [?Atom(r), ?Atom(RecordName)], ?S(_MapLine, map, {Var, KVs})), Str, Nl, Indent) ->
    fmt(Str, "#r.~p ~s#{~s}", Nl, Indent, [RecordName, print_single(Var), print_kvs(KVs)]);

print({kv, _L, Key, Val}, Str, Nl, Indent) ->
    fmt(Str, "~s: ~s", Nl, Indent, [print_single(Key), print_single(Val)]);

print({kvmatch, _L, Key, Val}, Str, Nl, Indent) ->
    fmt(Str, "~s := ~s", Nl, Indent, [print_single(Key), print_single(Val)]);

print(?T(_L, [?Atom(r), ?Atom(RecordName)], ?S(_MapLine, map, KVs)), Str, Nl, Indent) ->
    fmt(Str, "#r.~p {~s}", Nl, Indent, [RecordName, print_kvs(KVs)]);

print(?T(_L, [?Atom(c)], ?V(_StrLine, string, [Char])), Str, Nl, Indent) ->
    fmt(Str, "#c \"~c\"", Nl, Indent, [Char]);
% TODO: escape content of string
print(?T(_L, [?Atom(atom)], ?V(_StrLine, string, AtomStr)), Str, Nl, Indent) ->
    fmt(Str, "#atom \"~s\"", Nl, Indent, [AtomStr]);
print(?S(_L, tuple, []), Str, Nl, Indent)   ->
    fmt(Str, "#{}", Nl, Indent, []);
print(?S(_L, tuple, Val), Str, Nl, Indent)   ->
    fmt(Str, "{~s}", Nl, Indent, [print_seq(Val)]);
print(?S(_L, cons, {H, T}), Str, Nl, Indent) ->
    fmt(Str, "[~s::~s]", Nl, Indent, [print_single(H), print_single(T)]);

print(?V(_L, fn_ref, {{Mod, Fun}, Arity}), Str, Nl, Indent) ->
    fmt(Str, "fn:~s.~s:~s", Nl, Indent, [print_single(Mod), print_single(Fun), print_single(Arity)]);

print(?V(_L, fn_ref, {Fun, Arity}), Str, Nl, Indent) ->
    fmt(Str, "fn:~s:~s", Nl, Indent, [print_single(Fun), print_single(Arity)]);

print(?E(_L, 'when', Clauses), Str, _Nl, Indent) ->
    fmt(Str, "when ~s~send", true, Indent, [print_wclauses(Clauses, Indent),
                                           ind(Indent)]);

print({wcond, _L, Cond, Body}, Str, _Nl, Indent) ->
    fmt(Str, "else ~s:~n~s", false, Indent, [print_guard(Cond), print_body(Body, Indent + 1)]);

print({welse, _L, Body}, Str, _Nl, Indent) ->
    fmt(Str, "else:~n~s", false, Indent, [print_body(Body, Indent + 1)]);

print(?E(_L, 'for', {Qualifiers, Body}), Str, _Nl, Indent) ->
    fmt(Str, "for~s:~n~s~send", true, Indent,
        [print_qualifiers(Qualifiers, Indent + 1),
         print_body(Body, Indent + 1), ind(Indent)]);

print(?E(_L, 'try', {Body, Catch, After}), Str, Nl, Indent) ->
    fmt(Str, "try~n~s~s~s~send", true, Indent, [print_body(Body, Indent + 1),
                                     print_catch(Catch, Nl, Indent),
                                     print_after(After, Indent),
                                     ind(Indent)]);

print(?E(_L, 'receive', {?E(_CLine, 'case', Clauses), After}), Str, Nl, Indent) ->
    fmt(Str, "receive~n~s~s~send", true, Indent, [print_clauses(Clauses, Nl, Indent + 1),
                                              print_after(After, Indent),
                                              ind(Indent)]);

print(?E(_L, switch, {Value, ?E(_CaseLine, 'case', Clauses)}), Str, Nl, Indent) ->
    fmt(Str, "match ~s~n~s~send", true, Indent, [print_single(Value),
                                             print_clauses(Clauses, Nl, Indent + 1),
                                             ind(Indent)]);

print({cmatch, _L, {Conds, When, Body}}, Str, Nl, Indent) ->
    fmt(Str, "case ~s~s:~n~s", false, Indent,
        [print_conds(Conds), print_when(When), print(Body, "", Nl, Indent + 1)]);

print({celse, _L, Body}, Str, _Nl, Indent) ->
    fmt(Str, "else:~n~s", false, Indent, [print_body(Body, Indent + 1)]);

print(?E(_L, 'begin', Body), Str, _Nl, Indent) ->
    fmt(Str, "begin~n~s~send", true, Indent,
        [print_body(Body, Indent + 1), ind(Indent)]);

print(?E(_L, fn, {Name, Attrs, ?E(_CLine, 'case', Cases)}), Str, Nl, Indent) ->
    fmt(Str, "fn ~s~n~s~n~s~send", true, Indent,
        [print_single(Name), print(Attrs, "", Nl, Indent + 1),
         print_clauses(Cases, Nl, Indent + 1), ind(Indent)]);

print(?E(_L, fn, ?E(_CLine, 'case', Cases)), Str, Nl, Indent) ->
    fmt(Str, "fn~n~s~send", true, Indent,
        [print_clauses(Cases, Nl, Indent + 1), ind(Indent)]);

print(?E(_L, fn, {?V(_VLine, var, _RawName)=Name, ?E(_CLine, 'case', Cases)}), Str, Nl, Indent) ->
    fmt(Str, "fn ~s~n~s~send", true, Indent,
        [print_single(Name), print_clauses(Cases, Nl, Indent + 1), ind(Indent)]);

print(?E(_L, call, {{Mod, Fun}, Args}), Str, Nl, Indent) ->
    fmt(Str, "~s.~s(~s)", Nl, Indent,
        [print_single(Mod), print_single(Fun), print_seq(Args)]);

print(?E(_L, call, {Fun, Args}), Str, Nl, Indent) ->
    fmt(Str, "~s(~s)", Nl, Indent, [print_single(Fun), print_seq(Args)]);

print(?O(_L, Op, Left, Right), Str, Nl, Indent) ->
    WrapLeft = should_wrap(Op, left, Left),
    WrapRight = should_wrap(Op, right, Right),
    LeftFmt = if WrapLeft -> "(~s)"; true -> "~s" end,
    RightFmt = if WrapRight -> "(~s)"; true -> "~s" end,
    Format = LeftFmt ++ " ~s " ++ RightFmt,
    fmt(Str, Format, Nl, Indent,
        [print_single(Left), atom_to_list(Op), print_single(Right)]);

print(?V(_L, atom, Val), Str, Nl, Indent)    ->
    fmt(Str, "~s", Nl, Indent, [atom_to_list(Val)]);
print(?V(_L, integer, Val), Str, Nl, Indent) ->
    fmt(Str, "~p", Nl, Indent, [Val]);
print(?V(_L, float, Val), Str, Nl, Indent)   ->
    fmt(Str, "~p", Nl, Indent, [Val]);
print(?V(_L, boolean, Val), Str, Nl, Indent) ->
    fmt(Str, "~s", Nl, Indent, [atom_to_list(Val)]);
print(?V(_L, var, Val), Str, Nl, Indent)     ->
    fmt(Str, "~s", Nl, Indent, [atom_to_list(Val)]);
% TODO: escape string
print(?V(_L, string, Val), Str, Nl, Indent)  ->
    fmt(Str, "\"~s\"", Nl, Indent, [Val]);
% TODO: escape string
print(?V(_L, bstring, Val), Str, Nl, Indent) ->
    fmt(Str, "'~s'", Nl, Indent, [Val]);

print(?UO(_L, Op, Val), Str, Nl, Indent) ->
    fmt(Str, "~s~s", Nl, Indent, [atom_to_list(Op), print_single(Val)]);

print(Ast, Str, Nl, Indent) ->
    fmt(Str, "(INVALID: ~p)", Nl, Indent, [Ast]).


print([], Str, _Nl, _Indent, _Accum, _Sep) ->
    Str;
print([H], Str, Nl, Indent, _Accum, _Sep) ->
    print(H, Str, Nl, Indent);
print([H|T], Str, Nl, Indent, Accum, Sep) ->
    NewStr = [print(H, Str, Nl, Indent), Sep],
    print(T, NewStr, Nl, Indent, Accum, Sep).

print_kvs(Items) ->
    print(Items, "", false, 0, [], ", ").

print_seq(Items) ->
    print(Items, "", false, 0, [], ", ").

print_path(Items) ->
    print(Items, "", false, 0, [], ".").

print_wclauses([{wcond, _L, Cond, Body}|Clauses], Indent) ->
    [print_guard(Cond), ":\n", print_body(Body, Indent + 1), print(Clauses, "", true, Indent, [], "")].

print_catch(nocatch, _Nl, _Indent) -> "";
print_catch(?E(_CLine, 'case', Cases), Nl, Indent) ->
    fmt("", "catch~n~s", false, Indent, [print_clauses(Cases, Nl, Indent + 1)]).

print_after(noafter, _Indent) -> "";
print_after({Timeout, Body}, Indent) ->
    fmt("", "after ~s:~n~s", false, Indent, [print_single(Timeout),
                                             print_body(Body, Indent + 1)]);
print_after(Body, Indent) ->
    fmt("", "after:\n~s", false, Indent, [print_body(Body, Indent + 1)]).

print_clauses(Clauses, Nl, Indent) ->
    print(Clauses, "", Nl, Indent, [], "").

print_conds(Conds) ->
    print_seq(Conds).

print_body(Body, Indent) -> print(Body, "", true, Indent).

print_guard(Guard) ->
    string:join(lists:map(fun print_seq/1, Guard), "; ").

print_when(nowhen) -> "";
print_when(When) ->
    fmt("", " when ~s", false, 0, [print_guard(When)]).

print_thread_call({first, Call}, Indent) ->
    fmt("", "-> ~s", true, Indent, [print_single(Call)]);
print_thread_call({last, Call}, Indent) ->
    fmt("", "->> ~s", true, Indent, [print_single(Call)]).

print_thread_calls(Calls, Indent) ->
    lists:map(fun (TCall) -> print_thread_call(TCall, Indent) end, Calls).

print_qualifier({filter, Filter}, Indent) ->
    [ind(Indent), print_single(Filter)];
print_qualifier({generate, _Line, Left, Right}, Indent) ->
    fmt("", "~s in ~s", false, Indent, [print_single(Left), print_single(Right)]).

print_qualifiers(Qs, _Indent) ->
    StrQs = lists:map(fun (Q) -> print_qualifier(Q, 0) end, Qs),
    [" ", string:join(StrQs, "; ")].


precedence('~') -> {left, 300};
precedence('not') -> {left, 300};

precedence('andd') -> {left, 400};
precedence('&') -> {left, 400};
precedence('*') -> {left, 400};
precedence('/') -> {left, 400};
precedence('//') -> {left, 400};
precedence('%') -> {left, 400};

precedence('orr') -> {left, 500};
precedence('+') -> {left, 500};
precedence('-') -> {left, 500};
precedence('|') -> {left, 500};
precedence('^') -> {left, 500};
precedence('xor') -> {left, 500};
precedence('>>') -> {left, 500};
precedence('<<') -> {left, 500};

precedence('++') -> {right, 600};
precedence('--') -> {right, 600};

precedence('<') -> {left, 700};
precedence('<=') -> {left, 700};
precedence('>') -> {left, 700};
precedence('>=') -> {left, 700};
precedence('==') -> {left, 700};
precedence('===') -> {left, 700};
precedence('!=') -> {left, 700};
precedence('!==') -> {left, 700};

precedence('and') -> {left, 800};
precedence('or') -> {left, 900};

precedence('!') -> {right, 1000};
precedence('=') -> {right, 1000}.

should_wrap(Op, Pos, ?O(_Line, SubOp, _L, _R)) ->
    {OpAssoc, OpPrec} = precedence(Op),
    {SubOpAssoc, SubOpPrec} = precedence(SubOp),
    % TODO: this for sure is wrong
    case {Pos, OpAssoc, SubOpAssoc} of
        _ when OpPrec < SubOpPrec -> true;
        {left, left, _} when OpPrec == SubOpPrec -> true;
        {right, right, _} when OpPrec == SubOpPrec -> true;
        _ -> false
    end;
should_wrap(_Op, _Pos, _Sub) -> false.
