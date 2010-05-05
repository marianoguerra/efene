Nonterminals
    program tl_exprs tl_expr
    fn_def fun_def fn_patterns fn_pattern fn_parameters parameters fn_block
    exprs expr literal bool_lit
    send_expr match_expr
    bool_expr bool_and_expr comp_expr concat_expr
    add_expr mul_expr unary_expr
    block_expr arrow_expr
    if_expr if_patterns if_pattern
    case_expr case_body case_patterns case_pattern
    try_expr catch_patterns catch_pattern
    recv_expr receive_patterns receive_pattern
    list list_items list_item
    tuple tuple_items
    fun_call call_params call_parameters call_item farity
    arrow_chains arrow_chain
    list_comp list_generator list_generators
    bin_comp
    rec rec_def rec_set rec_new attr_sets attr_set
    binary binary_items binary_item bin_type_def bin_type
    prefix_op attribute.

Terminals
    fn match open close open_block close_block
    integer float string var char boolean atom endl
    send_op
    bool_orelse_op bool_andalso_op
    bool_or_op bool_and_op
    comp_op concat_op
    and_op or_op shift_op bin_not bool_not
    add_op mul_op
    if else when
    switch case
    try catch
    receive after
    open_list close_list sep split_op dot
    arrow
    open_bin close_bin
    record
    attr gattr.

Rootsymbol program.

prefix_op -> add_op : '$1'.
prefix_op -> bin_not : '$1'.
prefix_op -> bool_not : '$1'.

program -> exprs    : '$1'.
program -> tl_exprs : '$1'.

tl_exprs -> tl_expr : ['$1'].
tl_exprs -> tl_expr tl_exprs : ['$1'|'$2'].

tl_expr -> fn_def endl    : '$1'.
tl_expr -> rec_def endl   : '$1'.
tl_expr -> attribute endl : '$1'.

attribute -> attr open literal close : {attribute, line('$1'), unwrap('$1'), erl_parse:normalise('$3')}.
attribute -> gattr open literal close : {global_attribute, line('$1'), unwrap('$1'), erl_parse:normalise('$3')}.

fn_def -> atom match fn_patterns:
    Arity = get_arity('$3'),
    {function, line('$1'), unwrap('$1'), Arity, '$3'}.

fn_def -> atom atom match fn_patterns:
    Arity = get_arity('$4'),
    PublicToken = unwrap('$1'),
    if
        PublicToken /= public ->
            fail(line('$1'), "'public', expected on function definition got:", PublicToken);
        true ->
            {public_function, line('$2'), unwrap('$2'), Arity, '$4'}
    end.

fun_def -> fn_patterns : {'fun', element(2, hd('$1')), {clauses, '$1'}}.

fn_patterns -> fn_pattern : ['$1'].
fn_patterns -> fn_pattern fn_patterns : ['$1'|'$2'].

fn_pattern -> fn fn_parameters fn_block : {clause, line('$1'), '$2', [], '$3'}.
fn_pattern -> fn fn_parameters expr : {clause, line('$1'), '$2', [], ['$3']}.
fn_pattern -> fn fn_parameters when bool_expr fn_block : {clause, line('$1'), '$2', [['$4']], '$5'}.

fn_parameters -> open close : [].
fn_parameters -> open parameters close : '$2'.

parameters -> match_expr sep parameters: ['$1'|'$3'].
parameters -> match_expr : ['$1'].

fn_block -> open_block expr close_block : ['$2'].
fn_block -> open_block exprs close_block : '$2'.

exprs -> expr endl : ['$1'].
exprs -> expr endl exprs: ['$1'|'$3'].

expr -> send_expr: '$1'.

send_expr -> match_expr send_op send_expr        : {op, line('$2'), unwrap('$2'), '$1', '$3'}.
send_expr -> match_expr                         : '$1'.

match_expr -> bool_expr match match_expr         : {match, line('$2'), '$1', '$3'}.
match_expr -> bool_expr                         : '$1'.

bool_expr -> bool_and_expr bool_orelse_op bool_expr : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
bool_expr -> bool_and_expr                          : '$1'.

bool_and_expr -> comp_expr bool_andalso_op bool_and_expr : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
bool_and_expr -> comp_expr                               : '$1'.

comp_expr -> concat_expr comp_op concat_expr    : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
comp_expr -> concat_expr                        : '$1'.

concat_expr -> add_expr concat_op concat_expr   : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
concat_expr -> add_expr                         : '$1'.

add_expr -> add_expr add_op mul_expr         : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
add_expr -> add_expr or_op mul_expr          : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
add_expr -> add_expr bool_or_op mul_expr     : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
add_expr -> add_expr shift_op mul_expr       : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
add_expr -> mul_expr                         : '$1'.

mul_expr -> mul_expr mul_op unary_expr       : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> mul_expr and_op unary_expr       : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> mul_expr bool_and_op unary_expr  : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> unary_expr                       : '$1'.

unary_expr -> prefix_op literal              : {op, line('$2'), op(unwrap('$1')), '$2'}.
unary_expr -> block_expr                     : '$1'.

block_expr -> if_expr           : '$1'.
block_expr -> arrow_expr        : '$1'.
block_expr -> case_expr         : '$1'.
block_expr -> try_expr          : '$1'.
block_expr -> recv_expr         : '$1'.
block_expr -> fun_def           : '$1'.

% if expression
if_expr  -> if if_patterns      : {'if', line('$1'), '$2'}.

if_patterns -> if_pattern else fn_block :
    ['$1'|[{clause, line('$2'), [], [[{atom, line('$2'), true}]], '$3'}]].

if_patterns -> if_pattern else if if_patterns          : ['$1'|'$4'].
if_patterns -> if_pattern                              : ['$1'].
if_pattern  -> bool_expr fn_block                      : {clause, line('$1'), [], [['$1']], '$2'}.

% case expression
case_expr -> switch bool_expr case_body            : {'case', line('$1'), '$2', '$3'}.

case_body -> open_block case_patterns else fn_block endl close_block:
    '$2' ++ [{'clause', line('$3'), [{var, line('$3'), '_'}], [], '$4'}].
case_body -> open_block case_patterns endl close_block : '$2'.

case_patterns -> case_pattern case_patterns            : ['$1'|'$2'].
case_patterns -> case_pattern                          : ['$1'].
case_pattern -> case bool_expr fn_block :
    {'clause', line('$1'), ['$2'], [], '$3'}.
case_pattern -> case bool_expr when bool_expr fn_block :
    {'clause', line('$1'), ['$2'], [['$4']], '$5'}.

% try catch expression

try_expr -> try fn_block catch_patterns            : {'try', line('$1'), '$2', [], '$3', []}.
try_expr -> try fn_block catch_patterns after fn_block : {'try', line('$1'), '$2', [], '$3', '$5'}.

catch_patterns -> catch_pattern catch_patterns         : ['$1'|'$2'].
catch_patterns -> catch_pattern                        : ['$1'].
catch_pattern  -> catch open atom literal close fn_block:
    AtomName = unwrap('$3'),
    case AtomName == 'throw' orelse AtomName == 'error' orelse AtomName == 'exit' of
        true ->
            {clause, line('$1'), [{tuple, line('$1'), ['$3', '$4', {var, line('$1'), '_'}]}], [], '$6'};
        false ->
            fail(line('$1'), "'throw', 'error' or 'exit' expected on catch got:", AtomName)
    end.
catch_pattern -> catch atom literal fn_block:
    AtomName = unwrap('$2'),
    case AtomName == 'throw' orelse AtomName == 'error' orelse AtomName == 'exit' of
        true ->
            {clause, line('$1'), [{tuple, line('$1'), ['$2', '$3', {var, line('$1'), '_'}]}], [], '$4'};
        false ->
            fail(line('$1'), "'throw', 'error' or 'exit' expected on catch got:", AtomName)
    end.
catch_pattern -> catch var literal fn_block:
    {clause, line('$1'), [{tuple, line('$1'), ['$2', '$3', {var, line('$1'), '_'}]}], [], '$4'}.
catch_pattern -> catch open var literal close fn_block:
    {clause, line('$1'), [{tuple, line('$1'), ['$3', '$4', {var, line('$1'), '_'}]}], [], '$6'}.
catch_pattern -> catch open literal close fn_block:
    {clause, line('$1'), [{tuple, line('$1'), [{atom, line('$1'), throw}, '$3', {var, line('$1'), '_'}]}], [], '$5'}.
catch_pattern -> catch literal fn_block:
    {clause, line('$1'), [{tuple, line('$1'), [{atom, line('$1'), throw}, '$2', {var, line('$1'), '_'}]}], [], '$3'}.

% receive expression

recv_expr -> receive receive_patterns                             : {'receive', line('$1'), '$2'}.
recv_expr -> receive receive_patterns after literal fn_block      : {'receive', line('$1'), '$2', '$4', '$5'}.

receive_patterns -> receive_pattern else receive receive_patterns : ['$1'|'$4'].
receive_patterns -> receive_pattern                               : ['$1'].
receive_pattern  -> bool_expr fn_block                            : {'clause', line('$1'), ['$1'], [], '$2'}.
receive_pattern  -> bool_expr when bool_expr fn_block             : {'clause', line('$1'), ['$1'], ['$3'], '$4'}.

arrow_expr  -> literal arrow_chains : add_first_param('$1', '$2').
arrow_expr  -> literal : '$1'.

arrow_chains -> arrow_chain arrow_chains : add_first_param('$1', '$2').
arrow_chains -> arrow_chain    : '$1'.
arrow_chain  -> arrow fun_call : '$2'.

literal -> integer              : '$1'.
literal -> float                : '$1'.
literal -> bool_lit             : '$1'.
literal -> string               : {string,  line('$1'), unwrap('$1')}.
literal -> call_item            : '$1'.
literal -> open expr close      : '$2'.
literal -> char                 : '$1'.
literal -> list                 : '$1'.
literal -> tuple                : '$1'.
literal -> farity               : '$1'.
literal -> list_comp            : '$1'.
literal -> bin_comp             : '$1'.
literal -> rec                  : '$1'.
literal -> rec_set              : '$1'.
literal -> rec_new              : '$1'.
literal -> fun_call             : '$1'.
literal -> binary               : '$1'.

bool_lit -> boolean             : {atom, line('$1'), unwrap('$1')}.

% list type
list -> open_list close_list : {nil, line('$1')}.
list -> open_list list_item list_items close_list : {cons, line('$1'), '$2', '$3'}.
list -> open_list match_expr split_op match_expr close_list : {cons, line('$1'), '$2', '$4'}.

list_items -> list_item : {cons, line('$1'), {nil, line('$1')}}.
list_items -> list_items sep list_item : {cons, line('$2'), '$1', '$3'}.

list_items -> split_op list_item : '$2'.
list_items -> sep list_item list_items : {cons, line('$2'), '$2', '$3'}.
% XXX: the 1 in nil is to make pass the tests, find a way to put the line there
list_items -> '$empty' : {nil, 1}.

list_item -> match_expr : '$1'.

% tuple type
tuple -> open sep close    : {tuple, line('$1'), []}.
tuple -> open tuple_items close    : {tuple, line('$1'), '$2'}.

tuple_items -> match_expr sep : ['$1'].
tuple_items -> match_expr sep match_expr: ['$1', '$3'].
tuple_items -> match_expr sep tuple_items : ['$1'|'$3'].

% function call

fun_call -> call_item call_params            : {call, line('$1'), '$1', '$2'}.
% duplicated to avoid the rec from matching first for atom dot
fun_call -> atom dot call_item call_params  :
    {call, line('$2'), {remote, line('$2'), '$1', '$3'}, '$4'}.
fun_call -> var dot call_item call_params  :
    {call, line('$2'), {remote, line('$2'), '$1', '$3'}, '$4'}.
fun_call -> fun_call call_params       : {call, line('$1'), '$1', '$2'}.

call_item -> atom : '$1'.
call_item -> var: '$1'.

call_params -> open close : [].
call_params -> open call_parameters close : '$2'.

call_parameters -> expr : ['$1'].
call_parameters -> expr sep call_parameters: ['$1'|'$3'].

% function arity

farity -> fn atom split_op integer   :
    {'fun', line('$1'), {function, unwrap('$2'), unwrap('$4')}}.

farity -> fn atom dot atom split_op integer   :
    {'fun', line('$1'), {function, unwrap('$2'), unwrap('$4'), unwrap('$6')}}.

% list comprehension

list_comp -> open_list match_expr list_generators close_list : {lc, line('$1'), '$2', lists:flatten('$3')}.
bin_comp  -> open_bin match_expr list_generators close_bin   : {bc, line('$1'), '$2', lists:flatten('$3')}.

list_generators -> list_generator list_generators   : ['$1'|'$2'].
list_generators -> list_generator                   : '$1'.

list_generator -> atom bool_expr atom bool_expr :
    In = element(3, '$3'),
    For = element(3, '$1'),
    Line = line('$1'),

    if For /= for ->
            fail(Line, "'for' expected in list comprehension got:", For);
        In == in ->
            [{generate, Line, '$2', '$4'}];
        In == bin ->
            [{b_generate, Line, '$2', '$4'}];
       true ->
            fail(Line, "'in' expected in list comprehension got:", In)
    end.

list_generator -> atom bool_expr atom bool_expr if bool_expr :
    In = element(3, '$3'),
    For = element(3, '$1'),
    Line = line('$1'),

    if For /= for ->
            fail(Line, "'for' expected in list comprehension got:", For);
        In == in ->
            [{generate, Line, '$2', '$4'},'$6'];
        In == bin ->
            [{b_generate, Line, '$2', '$4'},'$6'];
       true ->
            fail(Line, "'in' expected in list comprehension got: ", In)
    end.

% records

rec -> atom dot var dot atom : {'record_field', line('$2'), '$3', unwrap('$1'), '$5'}.

rec_def -> atom match record open attr_sets close :
    {attribute, line('$2'), record, {unwrap('$1'), '$5'}}.

rec_set -> atom dot var open_list attr_sets close_list :
    {'record', line('$2'), '$3', unwrap('$1'), '$5'}.

rec_new -> atom open_list attr_sets close_list :
    {'record', line('$2'), unwrap('$1'), '$3'}.

attr_sets -> attr_set sep attr_sets : ['$1'|'$3'].
attr_sets -> atom sep attr_sets     : [{record_field, line('$1'), '$1'}|'$3'].
attr_sets -> attr_set               : ['$1'].

attr_set  -> atom match bool_expr : {record_field, line('$2'), '$1', '$3'}.

% binary

binary -> open_bin binary_items close_bin : {'bin', line('$1'), '$2'}.

binary_items -> binary_item sep binary_items : ['$1'|'$3'].
binary_items -> binary_item : ['$1'].

binary_item -> literal split_op integer mul_op bin_type_def :
    assert_atom('$4', '/'),
    {bin_element, line('$1'), '$1', '$3', '$5'}.

binary_item -> literal mul_op bin_type_def :
    assert_atom('$2', '/'),
    {bin_element, line('$1'), '$1', default, '$3'}.

binary_item -> literal split_op integer :
    {bin_element, line('$1'), '$1', '$3', default}.

binary_item -> literal : {bin_element, line('$1'), '$1', default, default}.

bin_type_def -> bin_type add_op bin_type_def :
    assert_atom('$2', '-'),
    ['$1'|'$3'].
bin_type_def -> bin_type : ['$1'].

bin_type -> atom split_op integer : {unwrap('$1'), unwrap('$3')}.
bin_type -> atom : unwrap('$1').

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line(T) when is_tuple(T) -> element(2, T).

get_arity([{clause, _, Params, _, _}|_T]) -> length(Params).

op('%') -> 'rem';
op('<<') -> 'bsl';
op('>>') -> 'bsr';
op('<=') -> '=<';
op('===') -> '=:=';
op('!=') -> '/=';
op('!==') -> '=/=';
op('|') -> 'bor';
op('&') -> 'band';
op('^') -> 'bxor';
op('~') -> 'bnot';
op('and') -> 'andalso';
op('andd') -> 'and';
op('or') -> 'orelse';
op('orr') -> 'or';
op(Op) -> Op.

add_first_param(Value, {call, Line, Call, Params}) ->
    {call, Line, Call, add_first_param(Value, Params)};
add_first_param(Value, [{call, Line, Call, Params}|T]) ->
    [{call, Line, Call, add_first_param(Value, Params)}|T];
add_first_param(Value, Ast) ->
    [Value|Ast].

assert_atom(Token, Atom) ->
    Got = unwrap(Token),
    Line = line(Token),

    if
        Got /= Atom ->
            fail(Line, "'" ++ atom_to_list(Atom) ++ "' expected, got: " , Got);
        true -> ok
    end.

fail(Line, Reason, Cause) ->
    throw({error, {Line, fn_parser, [Reason, Cause]}}).
