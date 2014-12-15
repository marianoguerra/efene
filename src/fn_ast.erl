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

-module(fn_ast).
-export([op/3, op/4,
         unary_op/2, unary_op/3,

         op_and/2, op_and/3, op_or/2, op_or/3,
         op_andd/2, op_andd/3, op_orr/2, op_orr/3, op_xor/2, op_xor/3,

         op_bor/2, op_bor/3, op_band/2, op_band/3, op_bxor/2, op_bxor/3,
         op_shl/2, op_shl/3, op_shr/2, op_shr/3,

         op_not/1, op_not/2, op_bnot/1, op_bnot/2,
         op_neg/1, op_neg/2, op_pos/1, op_pos/2,

         op_ladd/2, op_ladd/3, op_lsub/2, op_lsub/3,

         op_lt/2, op_lt/3, op_le/2, op_le/3,
         op_gt/2, op_gt/3, op_ge/2, op_ge/3,
         op_eq/2, op_eq/3, op_ne/2, op_ne/3,
         op_eeq/2, op_eeq/3, op_ene/2, op_ene/3,

         op_add/2, op_add/3, op_sub/2, op_sub/3,
         op_mul/2, op_mul/3, op_div/2, op_div/3,
         op_idiv/2, op_idiv/3, op_rem/2, op_rem/3,

         v_int/1, v_int/2,
         v_float/1, v_float/2,
         v_str/1, v_str/2,
         v_bool/1, v_bool/2,
         v_var/1, v_var/2,
         v_atom/1, v_atom/2,
         v_fn_ref/3, v_fn_ref/4,

         s_tuple/1, s_tuple/2,
         s_list/1, s_list/2,
         s_map/1, s_map/2,
         s_cons/2, s_cons/3,

         e_fn/2,
         e_case/2, e_case_match/3, e_case_match/4, e_case_else/2,
         e_when/1, e_when/2,
         e_when_cond/2, e_when_cond/3,
         e_when_else/1, e_when_else/2,

         call/2, call/3,
         call_f/2, call_f/3,
         call_mf/3, call_mf/4,

         expr/3,
         attr/2, attr/3, attr/4,
         kv/2, kv/3,
         tag/2, tag/3,

         op_match/2, op_match/3,
         op_send/2, op_send/3]).

op_match(Left, Right) -> op_match(1, Left, Right).
op_match(Line, Left, Right) -> op('=', Line, Left, Right).

op_send(Left, Right) -> op_send(1, Left, Right).
op_send(Line, Left, Right) -> op('!', Line, Left, Right).

op_and(Left, Right) -> op_and(1, Left, Right).
op_and(Line, Left, Right) -> op('and', Line, Left, Right).

op_or(Left, Right) -> op_or(1, Left, Right).
op_or(Line, Left, Right) -> op('or', Line, Left, Right).

op_xor(Left, Right) -> op_xor(1, Left, Right).
op_xor(Line, Left, Right) -> op('xor', Line, Left, Right).

op_andd(Left, Right) -> op_andd(1, Left, Right).
op_andd(Line, Left, Right) -> op('andd', Line, Left, Right).

op_orr(Left, Right) -> op_orr(1, Left, Right).
op_orr(Line, Left, Right) -> op('orr', Line, Left, Right).


op_add(Left, Right) -> op_add(1, Left, Right).
op_add(Line, Left, Right) -> op('+', Line, Left, Right).

op_sub(Left, Right) -> op_sub(1, Left, Right).
op_sub(Line, Left, Right) -> op('-', Line, Left, Right).

op_mul(Left, Right) -> op_mul(1, Left, Right).
op_mul(Line, Left, Right) -> op('*', Line, Left, Right).

op_div(Left, Right) -> op_div(1, Left, Right).
op_div(Line, Left, Right) -> op('/', Line, Left, Right).

op_idiv(Left, Right) -> op_idiv(1, Left, Right).
op_idiv(Line, Left, Right) -> op('//', Line, Left, Right).

op_rem(Left, Right) -> op_rem(1, Left, Right).
op_rem(Line, Left, Right) -> op('%', Line, Left, Right).


op_band(Left, Right) -> op_band(1, Left, Right).
op_band(Line, Left, Right) -> op('&', Line, Left, Right).

op_bor(Left, Right) -> op_bor(1, Left, Right).
op_bor(Line, Left, Right) -> op('|', Line, Left, Right).

op_bxor(Left, Right) -> op_bxor(1, Left, Right).
op_bxor(Line, Left, Right) -> op('^', Line, Left, Right).

op_shl(Left, Right) -> op_shl(1, Left, Right).
op_shl(Line, Left, Right) -> op('<<', Line, Left, Right).

op_shr(Left, Right) -> op_shr(1, Left, Right).
op_shr(Line, Left, Right) -> op('>>', Line, Left, Right).


op_ladd(Left, Right) -> op_ladd(1, Left, Right).
op_ladd(Line, Left, Right) -> op('++', Line, Left, Right).

op_lsub(Left, Right) -> op_lsub(1, Left, Right).
op_lsub(Line, Left, Right) -> op('--', Line, Left, Right).


op_lt(Left, Right) -> op_lt(1, Left, Right).
op_lt(Line, Left, Right) -> op('<', Line, Left, Right).

op_le(Left, Right) -> op_le(1, Left, Right).
op_le(Line, Left, Right) -> op('<=', Line, Left, Right).

op_gt(Left, Right) -> op_gt(1, Left, Right).
op_gt(Line, Left, Right) -> op('>', Line, Left, Right).

op_ge(Left, Right) -> op_ge(1, Left, Right).
op_ge(Line, Left, Right) -> op('>=', Line, Left, Right).

op_eq(Left, Right) -> op_eq(1, Left, Right).
op_eq(Line, Left, Right) -> op('==', Line, Left, Right).

op_ne(Left, Right) -> op_ne(1, Left, Right).
op_ne(Line, Left, Right) -> op('!=', Line, Left, Right).

op_eeq(Left, Right) -> op_eeq(1, Left, Right).
op_eeq(Line, Left, Right) -> op('===', Line, Left, Right).

op_ene(Left, Right) -> op_ene(1, Left, Right).
op_ene(Line, Left, Right) -> op('!==', Line, Left, Right).


op_not(Val) -> op_not(1, Val).
op_not(Line, Val) -> unary_op('not', Line, Val).

op_bnot(Val) -> op_bnot(1, Val).
op_bnot(Line, Val) -> unary_op('~', Line, Val).

op_neg(Val) -> op_neg(1, Val).
op_neg(Line, Val) -> unary_op('-', Line, Val).

op_pos(Val) -> op_pos(1, Val).
op_pos(Line, Val) -> unary_op('+', Line, Val).

v_int(Val) -> v_int(1, Val).
v_int(Line, Val) -> v(Line, integer, Val).

v_float(Val) -> v_float(1, Val).
v_float(Line, Val) -> v(Line, float, Val).

v_str(Val) -> v_str(1, Val).
v_str(Line, Val) -> v(Line, string, Val).

v_bool(Val) -> v_bool(1, Val).
v_bool(Line, Val) -> v(Line, boolean, Val).

v_atom(Val) -> v_atom(1, Val).
v_atom(Line, Val) -> v(Line, atom, Val).

v_fn_ref(Line, FName, Arity) -> {val, Line, fn_ref, {FName, Arity}}.
v_fn_ref(Line, ModName, FName, Arity) -> {val, Line, fn_ref, {{ModName, FName}, Arity}}.

v_var(Val) -> v_var(1, Val).
v_var(Line, Val) -> v(Line, var, Val).


s_tuple(Val) -> s_tuple(1, Val).
s_tuple(Line, Val) -> seq(Line, tuple, Val).

s_list(Val) -> s_list(1, Val).
s_list(Line, Val) -> seq(Line, list, Val).

s_map(Val) -> s_map(1, Val).
s_map(Line, Val) -> seq(Line, map, Val).

s_cons(Head, Tail) -> s_cons(1, Head, Tail).
s_cons(Line, Head, Tail) -> seq(Line, cons, {Head, Tail}).

e_fn(Line, Cases) -> expr(Line, fn, Cases).

e_case(Line, Matches) -> expr(Line, 'case', Matches).

e_case_match(Line, Match, Body) -> e_case_match(Line, Match, nowhen, Body).
e_case_match(Line, Match, When, Body) -> {cmatch, Line, {Match, When, Body}}.

e_case_else(Line, Body) -> {celse, Line, Body}.

e_when(Conds) -> e_when(1, Conds).
e_when(Line, Conds) -> expr(Line, 'when', Conds).

e_when_cond(Cond, Body) -> e_when_cond(1, Cond, Body).
e_when_cond(Line, Cond, Body) -> {wcond, Line, Cond, Body}.

e_when_else(Body) -> e_when_else(1, Body).
e_when_else(Line, Body) -> {welse, Line, Body}.

call_f(Fun, Args) -> call_f(1, Fun, Args).
call_f(Line, Fun, Args) -> call(Line, Fun, Args).

call_mf(Mod, Fun, Args) -> call_mf(1, Mod, Fun, Args).
call_mf(Line, Mod, Fun, Args) -> call(Line, {Mod, Fun}, Args).

call(MF, Args) -> call(1, MF, Args).
call(Line, MF, Args) -> expr(Line, call, {MF, Args}).

expr(Line, Type, Args) -> {expr, Line, Type, Args}.

tag(Tag, Val) -> tag(1, Tag, Val).
tag(Line, Tag, Val) -> {tag, Line, Tag, Val}.

kv(Key, Val) -> kv(1, Key, Val).
kv(Line, Key, Val) -> {kv, Line, Key, Val}.

v(Line, Type, Val) -> {val, Line, Type, Val}.

seq(Line, Type, Val) -> {seq, Line, Type, Val}.

attr(Line, Name) -> attr(Line, Name, noparams, noresult).
attr(Line, Name, Params) -> attr(Line, Name, Params, noresult).
attr(Line, Name, Params, Result) -> attr(Line, Name, Params, Result).

op(Op, Left, Right) ->
    op(Op, 1, Left, Right).

op(Op, Line, Left, Right) ->
    {op, Line, Op, Left, Right}.

unary_op(Op, Val) ->
    op(Op, 1, Val).

unary_op(Op, Line, Val) ->
    {unary_op, Line, Op, Val}.
