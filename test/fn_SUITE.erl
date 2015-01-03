-module(fn_SUITE).
-compile(export_all).

all() -> [lex_tl_fn, parse_tl_fn, parse_switch, parse_when, parse_begin,
          parse_receive, parse_try,
          parse_tuple, parse_list, parse_cons, parse_map,
          parse_tagged, parse_fn, parse_fn_ref, parse_call, parse_call_do, parse_call_thread,
          parse_match, parse_send, parse_bool_op, parse_comp_op, parse_concat_op,
          parse_add_op, parse_mul_op, parse_unary_op, parse_nested_op].

init_per_suite(Config) ->
    Config.

print(Thing) ->
    ct:print("~p~n", [Thing]).

lex_tl_fn(_) ->
    print(efene:tokenize("fn add: ok end")).

parse(Code) ->
    print(Code),
    print(efene:parse(Code)).

check_expr(Code, Ast) ->
    CodeAst = parse_expr(Code),
    case CodeAst of
        Ast -> ok; % ct:print("ok: ~s", [Code]);
        Other -> ct:print("error: ~s~nast: ~p~n  != parsed: ~n~p", [Code, Ast, Other])
    end.

parse_expr(Code) ->
    parse_expr(Code, false).

parse_expr(Code, PrintAst) ->
    case efene:parse("(" ++ Code ++ ")") of
        {ok, [Ast]} ->
            if PrintAst -> ct:print("~s~n~n~p", [Code, Ast]);
               true -> ok
            end,
            Ast;
        Other ->
            ct:print("error: parsing ~s: ~p", [Code, Other]),
            Other
    end.

parse_tl_fn(_) ->
    parse("fn add: case ok: yes\nend"),
    parse("fn add: case ok: yes\nelse: no\nend"),
    parse("fn add: case ok: yes\ncase soso: mmh\nelse: no\nend"),
    parse("fn add: case ok: yes\ncase soso: mmh\ncase foo: bar\nelse: no\nend"),
    parse("fn add: case ok, A: A\ncase error, Reason: Reason\nelse: no\nend"),
    parse("fn add: case ok when 42: yes\ncase soso: mmh\ncase foo when true: bar\nelse: no\nend"),

    parse("fn add: @public\ncase ok: yes\nend"),
    parse("fn add: @public\n@since \"1.2\" \ncase ok: yes\nend"),
    parse("fn add: @public\n@since \"1.2\" \n@spec #(foo, bar)  -> baz\ncase ok: yes\nend").

parse_switch(_) ->
    parse_expr("switch A: case a: b\nend"),
    parse_expr("switch A: case a: b\nelse: c\nend"),
    parse_expr("switch A: case a: b\ncase b: c\nelse: d\nend").

parse_when(_) ->
    check_expr("when A: a\nend", w(1, [wc(1, v('A'), [a(a)])])),
    check_expr("when A: a\nelse: z\nend", w(1, [wc(1, v('A'), [a(a)]),
                                                we(2, [a(2, z)])])),
    check_expr("when A: a\nelse B: b\nend",
               w(1, [wc(1, v('A'), [a(a)]),
                     wc(2, v(2, 'B'), [a(2, b)])])),
    check_expr("when A: a\nelse B: b\nelse: z\nend",
               w(1, [wc(1, v('A'), [a(a)]),
                     wc(2, v(2, 'B'), [a(2, b)]),
                     we(3, [a(3, z)])])),
    check_expr("when A: a\nelse B: b\nelse C: c\nend",
               w(1, [wc(1, v('A'), [a(a)]),
                     wc(2, v(2, 'B'), [a(2, b)]),
                     wc(3, v(3, 'C'), [a(3, c)])])),
    check_expr("when A: a\nelse B: b\nelse C: c\nelse: z\nend",
               w(1, [wc(1, v('A'), [a(a)]),
                     wc(2, v(2, 'B'), [a(2, b)]),
                     wc(3, v(3, 'C'), [a(3, c)]),
                     we(4, [a(4, z)])])).

parse_begin(_) ->
    parse_expr("begin a\nend"),
    parse_expr("begin a\nb\nend").

parse_receive(_) ->
    parse_expr("receive case A: a\nend"),
    parse_expr("receive case A: a\ncase B: b\nend"),
    parse_expr("receive case A: a\ncase B: b\nelse: c\nend"),
    parse_expr("receive case A: a\nafter 1: ohno\nend"),
    parse_expr("receive case A: a\ncase B: b\nafter 2: ohno\nend"),
    parse_expr("receive case A: a\ncase B: b\nelse: c\nafter 3: ohno\nend").

parse_try(_) ->
    parse_expr("try a\ncatch case A: a\nend"),
    parse_expr("try a\ncatch case A: a\nelse: b\nend"),
    parse_expr("try a\ncatch case A: a\ncase B: b\nelse: c\nend"),

    parse_expr("try a\ncatch case A: a\nafter: aft\nend"),
    parse_expr("try a\ncatch case A: a\nelse: b\nafter: aft\nend"),
    parse_expr("try a\ncatch case A: a\ncase B: b\nelse: c\nafter: aft\nend"),

    parse_expr("try a\nafter: aft\nend").

t(Vals) -> fn_ast:s_tuple(Vals).
l(Vals) -> fn_ast:s_list(Vals).
m(Vals) -> fn_ast:s_map(Vals).
c(H, T) -> fn_ast:s_cons(H, T).
kv(K, V) -> fn_ast:kv(K, V).

i(V) -> fn_ast:v_int(V).
i(Line, V) -> fn_ast:v_int(Line, V).
f(V) -> fn_ast:v_float(V).
a(V) -> fn_ast:v_atom(V).
a(Line, V) -> fn_ast:v_atom(Line, V).
v(V) -> fn_ast:v_var(V).
v(Line, V) -> fn_ast:v_var(Line, V).

true() -> fn_ast:v_bool(true).
false() -> fn_ast:v_bool(false).

w(Line, Conds) -> fn_ast:e_when(Line, Conds).
wc(Line, Cond, Body) -> fn_ast:e_when_cond(Line, Cond, Body).
we(Line, Body) -> fn_ast:e_when_else(Line, Body).

tag(T, V) -> fn_ast:tag(T, V).
call(MF, Args) -> fn_ast:call(1, MF, Args).

and_(Left, Right) -> fn_ast:op_and(Left, Right).
andd(Left, Right) -> fn_ast:op_andd(Left, Right).
or_(Left, Right) -> fn_ast:op_or(Left, Right).
orr(Left, Right) -> fn_ast:op_orr(Left, Right).
xor_(Left, Right) -> fn_ast:op_xor(Left, Right).

gt(Left, Right) -> fn_ast:op_gt(Left, Right).
ge(Left, Right) -> fn_ast:op_ge(Left, Right).
lt(Left, Right) -> fn_ast:op_lt(Left, Right).
le(Left, Right) -> fn_ast:op_le(Left, Right).
eq(Left, Right) -> fn_ast:op_eq(Left, Right).
ne(Left, Right) -> fn_ast:op_ne(Left, Right).
eeq(Left, Right) -> fn_ast:op_eeq(Left, Right).
ene(Left, Right) -> fn_ast:op_ene(Left, Right).

e_fn(Cases) -> fn_ast:e_fn(1, Cases).

e_case(Cases) when not is_list(Cases) -> e_case([Cases]);
e_case(Cases) -> fn_ast:e_case(1, Cases).

cmatch(Args, Body) -> fn_ast:e_case_match(1, Args, Body).

parse_tuple(_) ->
    check_expr("#()", t([])),
    check_expr("#(1)", t([i(1)])),
    check_expr("#(1, 2)", t([i(1), i(2)])),
    check_expr("#(1, 2, 3)", t([i(1), i(2), i(3)])),
    check_expr("#(1,)", t([i(1)])),
    check_expr("#(1, 2,)", t([i(1), i(2)])),
    check_expr("#(1, 2, 3,)", t([i(1), i(2), i(3)])).

parse_list(_) ->
    check_expr("[]", l([])),
    check_expr("[1]", l([i(1)])),
    check_expr("[1, 2]", l([i(1), i(2)])),
    check_expr("[1, 2, 3]", l([i(1), i(2), i(3)])),
    check_expr("[1,]", l([i(1)])),
    check_expr("[1, 2,]", l([i(1), i(2)])),
    check_expr("[1, 2, 3,]", l([i(1), i(2), i(3)])).

parse_cons(_) ->
    check_expr("[1::a]", c(i(1), a(a))),
    check_expr("[1::A]", c(i(1), v('A'))),
    check_expr("[1::[1,2,3]]", c(i(1), l([i(1), i(2), i(3)]))),
    check_expr("[[]::[1,2,3]]", c(l([]), l([i(1), i(2), i(3)]))).

parse_map(_) ->
    check_expr("{}", m([])),
    check_expr("{a: 1}", m([kv(a(a), i(1))])),
    check_expr("{a: 1, b: 2}", m([kv(a(a), i(1)), kv(a(b), i(2))])),
    check_expr("{a: 1, b: 2, c: [1,2,3]}", m([kv(a(a), i(1)),
                                              kv(a(b), i(2)),
                                              kv(a(c), l([i(1), i(2), i(3)]))])),

    check_expr("{}", m([])),
    check_expr("{a: 1,}", m([kv(a(a), i(1))])),
    check_expr("{a: 1, b: 2,}", m([kv(a(a), i(1)), kv(a(b), i(2))])),
    check_expr("{a: 1, b: 2, c: [1,2,3],}", m([kv(a(a), i(1)),
                                              kv(a(b), i(2)),
                                              kv(a(c), l([i(1), i(2), i(3)]))])).

parse_tagged(_) ->
    check_expr("#a 1", tag(a, i(1))),
    check_expr("#a 1.2", tag(a, f(1.2))),
    check_expr("#a true", tag(a, true())),
    check_expr("#a []", tag(a, l([]))),
    check_expr("#a #()", tag(a, t([]))),
    check_expr("#a {}", tag(a, m([]))).

parse_fn(_) ->
    check_expr("fn case 1: 2\nend",
              e_fn(e_case(cmatch([i(1)], [i(2)])))),
    parse_expr("fn case 1 when false: 2\nend"),
    parse_expr("fn case 1: 2\ncase 2: 3\nelse: 4\nend"),
    parse_expr("fn case 1: 2\nelse: 3\nend").

parse_call(_) ->
    check_expr("a()", call(a(a), [])),
    check_expr("a(1)", call(a(a), [i(1)])),
    check_expr("a(1, 2)", call(a(a), [i(1), i(2)])),
    check_expr("a(1, 2, 3)", call(a(a), [i(1), i(2), i(3)])),

    check_expr("a()", call(a(a), [])),
    check_expr("a(1,)", call(a(a), [i(1)])),
    check_expr("a(1, 2,)", call(a(a), [i(1), i(2)])),
    check_expr("a(1, 2, 3,)", call(a(a), [i(1), i(2), i(3)])),

    check_expr("a.b()", fn_ast:call_mf(a(a), a(b), [])),
    check_expr("a.B()", fn_ast:call_mf(a(a), v('B'), [])),
    check_expr("A.b()", fn_ast:call_mf(v('A'), a(b), [])),
    check_expr("A.B()", fn_ast:call_mf(v('A'), v('B'), [])).

parse_call_thread(_) ->
    parse_expr("42 -> a()"),
    parse_expr("42 ->> a()"),
    parse_expr("42 -> a(1)"),
    parse_expr("42 ->> a(1, 2)"),
    parse_expr("A -> a() -> b(1)"),
    parse_expr("A -> a() -> b(1) ->> c(2, 3)"),
    parse_expr("42 ->> a() -> b(a, 1) ->> c([1,2,3])").

parse_call_do(_) ->
    parse_expr("a.b() <- fn case 1: 1\nend", true),
    parse_expr("a.B(1) <- fn case 1: 1\ncase 2 when true: 2\nend", true),
    parse_expr("A(1, 2) <- fn case 1: 1\ncase 2 when true: 2\ncase 3: asd\nend", true),
    parse_expr("A.B(1, 2, 3) <- fn case 1: 1\ncase 2 when true: 2\ncase 3: asd\nelse: lala\nend", true),

    parse_expr("a.b() <<- fn case 1: 1\nend", true),
    parse_expr("a.B(1) <<- fn case 1: 1\ncase 2 when true: 2\nend", true),
    parse_expr("A(1, 2) <<- fn case 1: 1\ncase 2 when true: 2\ncase 3: asd\nend", true),
    parse_expr("A.B(1, 2, 3) <<- fn case 1: 1\ncase 2 when true: 2\ncase 3: asd\nelse: lala\nend", true).

parse_match(_) ->
    check_expr("a = A", fn_ast:op_match(a(a), v('A'))),
    check_expr("A = 1", fn_ast:op_match(v('A'), i(1))),
    check_expr("#(ok, Foo) = do_stuff(42)", fn_ast:op_match(t([a(ok), v('Foo')]), call(a(do_stuff), [i(42)]))),
    check_expr("#(ok, Foo) = when 1: 2\nelse: 3\nend",
               fn_ast:op_match(t([a(ok), v('Foo')]), 
                              w(1, [wc(1, i(1), [i(2)]), we(2, [i(2, 3)])]))).

parse_fn_ref(_) ->
    check_expr("fn:foo:4", fn_ast:v_fn_ref(1, a(foo), i(4))),
    check_expr("fn:foo.bar:4", fn_ast:v_fn_ref(1, a(foo), a(bar), i(4))),
    check_expr("fn:foo.Bar:4", fn_ast:v_fn_ref(1, a(foo), v('Bar'), i(4))),
    check_expr("fn:Foo.Bar:4", fn_ast:v_fn_ref(1, v('Foo'), v('Bar'), i(4))),
    check_expr("A = fn:foo:4", fn_ast:op_match(v('A'), fn_ast:v_fn_ref(1, a(foo), i(4)))),
    check_expr("lists.map(fn:foo:4, List)",
               call({a(lists), a(map)}, [fn_ast:v_fn_ref(1, a(foo), i(4)), v('List')])).

parse_send(_) ->
    check_expr("a ! #(ok, 1)", fn_ast:op_send(a(a), t([a(ok), i(1)]))),
    check_expr("A ! 1", fn_ast:op_send(v('A'), i(1))),
    check_expr("a ! do_stuff(42)", fn_ast:op_send(a(a), call(a(do_stuff), [i(42)]))),
    check_expr("B ! Asd", fn_ast:op_send(v('B'), v('Asd'))).

parse_bool_op(_) ->
    check_expr("true", true()),
    check_expr("true and false", and_(true(), false())),
    check_expr("true and false and true", and_(true(), and_(false(), true()))),
    check_expr("true or false and true", or_(true(), and_(false(), true()))),
    check_expr("true and false or true", or_(and_(true(), false()), true())),
    check_expr("true and false or true and false", or_(and_(true(), false()), and_(true(), false()))).

parse_comp_op(_) ->
    check_expr("1 < 2", lt(i(1), i(2))),
    check_expr("1 > 2", gt(i(1), i(2))),
    check_expr("1 <= 2", le(i(1), i(2))),
    check_expr("1 >= 2", ge(i(1), i(2))),
    check_expr("1 == 2", eq(i(1), i(2))),
    check_expr("1 is 2", eeq(i(1), i(2))),
    check_expr("1 isnt 2", ene(i(1), i(2))),
    check_expr("1 != 2", ne(i(1), i(2))),
    check_expr("1 < 2 and 3 > 4 or 5 != 6 and 7 == 8",
              or_(and_(lt(i(1), i(2)), gt(i(3), i(4))),
                  and_(ne(i(5), i(6)), eq(i(7), i(8))))).

parse_concat_op(_) ->
    check_expr("[] ++ A", fn_ast:op_ladd(l([]), v('A'))),
    check_expr("[1, 2] -- B", fn_ast:op_lsub(l([i(1), i(2)]), v('B'))).

parse_add_op(_) ->
    check_expr("1 + 2", fn_ast:op_add(i(1), i(2))),
    check_expr("1 - 2", fn_ast:op_sub(i(1), i(2))),
    check_expr("1 ^ 2", fn_ast:op_bxor(i(1), i(2))),
    check_expr("1 << 2", fn_ast:op_shl(i(1), i(2))),
    check_expr("1 >> 2", fn_ast:op_shr(i(1), i(2))),
    check_expr("true orr false", orr(true(), false())),
    check_expr("true xor false", xor_(true(), false())).

parse_mul_op(_) ->
    check_expr("1 * 2", fn_ast:op_mul(i(1), i(2))),
    check_expr("1 / 2", fn_ast:op_div(i(1), i(2))),
    check_expr("1 // 2", fn_ast:op_idiv(i(1), i(2))),
    check_expr("1 % 2", fn_ast:op_rem(i(1), i(2))),
    check_expr("1 & 2", fn_ast:op_band(i(1), i(2))),
    check_expr("1 andd 2", andd(i(1), i(2))).

parse_unary_op(_) ->
    check_expr("+ 1 * - 2", fn_ast:op_mul(fn_ast:op_pos(i(1)), fn_ast:op_neg(i(2)))),
    check_expr("+ 1 * - 2 + ~3 and not false",
               and_(fn_ast:op_add(
                      fn_ast:op_mul(fn_ast:op_pos(i(1)), fn_ast:op_neg(i(2))),
                      fn_ast:op_bnot(i(3))),
                    fn_ast:op_not(false()))),
    check_expr("not (a())", fn_ast:op_not(call(a(a), []))).

parse_nested_op(_) ->
    check_expr("1 * (2 + 3) / 4",
              fn_ast:op_mul(i(1), fn_ast:op_div(fn_ast:op_add(i(2), i(3)), i(4)))),
    check_expr("1 and (2 or 3) and 4",
              fn_ast:op_and(i(1), fn_ast:op_and(fn_ast:op_or(i(2), i(3)), i(4)))).
