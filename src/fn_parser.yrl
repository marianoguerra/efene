Nonterminals
    program tl_exprs tl_expr fn_def fun_def fn_patterns fn_pattern
    fn_parameters parameters fn_block exprs literal bool_lit send_expr
    match_expr def_expr bool_expr bool_and_expr comp_expr concat_expr add_expr
    mul_expr block_expr arrow_expr when_expr when_patterns when_pattern if_expr
    case_expr case_body case_patterns case_pattern try_expr catch_patterns
    catch_pattern recv_expr receive_patterns receive_pattern list list_items
    tuple tuple_items fun_call farity arrow_chains arrow_chain list_comp
    list_generator list_generators bin_comp rec rec_set rec_new attr_sets
    attr_set binary binary_items binary_item bin_type_def bin_type prefix_op
    attribute for_expr range signed_integer meta_block astify meta_astify attrs.

Terminals
    fn match open close open_block close_block integer float string var char
    boolean atom endl send_op bool_orelse_op bool_andalso_op bool_or_op
    bool_and_op comp_op concat_op and_op or_op shift_op bin_not bool_not add_op
    mul_op if else when switch case try catch receive after open_list
    close_list sep split_op split_def_op dot dotdot dotdotdot arrow open_bin
    close_bin attr gattr for in open_meta_block open_oxford close_oxford
    open_meta_oxford.

Rootsymbol program.

Expect 5.

Left 50 arrow.
Left 100 bool_orelse_op.
Left 200 bool_andalso_op.
Left 300 comp_op.
Right 400 concat_op.
Left 500 and_op.
Left 700 or_op.
Left 700 add_op bool_or_op.
Left 800 shift_op.
Left 800 mul_op bool_and_op.
Left 900 bin_not.
Left 900 bool_not.
Left 1000 match send_op.
Left 1100 open.


prefix_op -> add_op : '$1'.
prefix_op -> bin_not : '$1'.
prefix_op -> bool_not : '$1'.

program -> exprs    : '$1'.
program -> tl_exprs : '$1'.

tl_exprs -> tl_expr : ['$1'].
tl_exprs -> tl_expr tl_exprs : ['$1'|'$2'].

tl_expr -> fn_def endl    : '$1'.
tl_expr -> attribute endl : '$1'.
tl_expr -> send_op meta_block endl  : '$2'.
tl_expr -> send_op astify endl      : '$2'.
tl_expr -> send_op meta_astify endl : '$2'.

attribute -> attrs :
    run_attribute(attr_level('$1'), unwrap('$1'), line('$1'), []).

attribute -> attrs dot atom :
    run_attribute(attr_level('$1'), unwrap('$1'), unwrap('$3'), line('$1'), []).

attribute -> attrs open parameters close :
    run_attribute(attr_level('$1'), unwrap('$1'), line('$1'), '$3').

attribute -> attrs dot atom open parameters close :
    run_attribute(attr_level('$1'), unwrap('$1'), unwrap('$3'), line('$1'), '$5').

attribute -> attrs open parameters close arrow send_expr :
    run_attribute(attr_level('$1'), unwrap('$1'), line('$1'), {return, '$3', '$6'}).

attribute -> attrs dot atom open parameters close arrow send_expr :
    run_attribute(attr_level('$1'), unwrap('$1'), unwrap('$3'), line('$1'), {return, '$5', '$8'}).

attrs -> attr  : '$1'.
attrs -> gattr : '$1'.

fn_def -> atom match fn_patterns:
    Arity = get_arity('$3'),
    {function, line('$1'), unwrap('$1'), Arity, '$3'}.

fun_def -> fn_patterns : {'fun', element(2, hd('$1')), {clauses, '$1'}}.

fn_patterns -> fn_pattern : ['$1'].
fn_patterns -> fn_pattern fn_patterns : ['$1'|'$2'].

fn_pattern -> fn fn_parameters fn_block : {clause, line('$1'), '$2', [], '$3'}.
fn_pattern -> fn fn_parameters when bool_expr fn_block : {clause, line('$1'), '$2', [['$4']], '$5'}.

fn_parameters -> open close : [].
fn_parameters -> open parameters close : '$2'.

parameters -> send_expr sep parameters: ['$1'|'$3'].
parameters -> send_expr : ['$1'].

fn_block -> open_block send_expr close_block : ['$2'].
fn_block -> open_block exprs close_block : '$2'.

meta_block  -> open_meta_block  bool_expr close         :
    Type = element(1, '$2'),

    if
        Type == var ->
            list_to_atom([$$|atom_to_list(unwrap('$2'))]);
        true ->
            fn_meta:eval('$2')
    end.
astify      -> open_oxford      bool_expr close_oxford  : fn_meta:astify(line('$1'), '$2').
meta_astify -> open_meta_oxford bool_expr close_list    : fn_meta:astify(line('$1'), fn_meta:eval('$2')).

exprs -> send_expr endl : ['$1'].
exprs -> send_expr endl exprs: ['$1'|'$3'].

send_expr -> match_expr send_op send_expr        : {op, line('$2'), unwrap('$2'), '$1', '$3'}.
send_expr -> match_expr                         : '$1'.

match_expr -> def_expr match match_expr         : {match, line('$2'), '$1', '$3'}.
match_expr -> def_expr                         : '$1'.

def_expr -> literal split_def_op bool_expr : {def, line('$2'), '$1', '$3'}.
def_expr -> bool_expr : '$1'.


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

mul_expr -> mul_expr mul_op block_expr       : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> mul_expr and_op block_expr       : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> mul_expr bool_and_op block_expr  : {op, line('$2'), op(unwrap('$2')), '$1', '$3'}.
mul_expr -> block_expr                       : '$1'.

signed_integer -> add_op integer : {op, line('$2'), unwrap('$1'), '$2'}.

signed_integer -> integer : '$1'.

block_expr -> if_expr           : '$1'.
block_expr -> for_expr          : '$1'.
block_expr -> when_expr         : '$1'.
block_expr -> arrow_expr        : '$1'.
block_expr -> case_expr         : '$1'.
block_expr -> try_expr          : '$1'.
block_expr -> recv_expr         : '$1'.
block_expr -> fun_def           : '$1'.

% when expression
when_expr  -> when when_patterns      : {'if', line('$1'), '$2'}.

when_patterns -> when_pattern else fn_block :
    ['$1'|[{clause, line('$2'), [], [[{atom, line('$2'), true}]], '$3'}]].

when_patterns -> when_pattern else when when_patterns          : ['$1'|'$4'].
when_patterns -> when_pattern                              : ['$1'].
when_pattern  -> bool_expr fn_block                      : {clause, line('$1'), [], [['$1']], '$2'}.

% if expression

if_expr     -> if bool_expr fn_block :
     {'case', line('$1'), '$2', [{clause, line('$3'), [{atom, line('$3'), true}], [], '$3'}]}.

if_expr     -> if bool_expr fn_block else if_expr :
     {'case', line('$1'), '$2',
         [{clause, line('$3'), [{atom, line('$3'), true}], [], '$3'},
          {clause, line('$3'), [{atom, line('$3'), false}], [], ['$5']}]}.

if_expr     -> if bool_expr fn_block else fn_block:
     {'case', line('$1'), '$2',
         [{clause, line('$3'), [{atom, line('$3'), true}], [], '$3'},
          {clause, line('$3'), [{atom, line('$3'), false}], [], '$5'}]}.

% for expression

for_expr -> for match_expr in bool_expr fn_block :
    {lc, line('$1'), {block, line('$1'), '$5'}, [{generate, line('$3'), '$2', '$4'}]}.

for_expr -> for open match_expr in bool_expr close fn_block :
    {lc, line('$1'), {block, line('$1'), '$7'}, [{generate, line('$4'), '$3', '$5'}]}.

for_expr -> for match_expr in bool_expr if bool_expr fn_block :
    {lc, line('$1'), {block, line('$1'), '$7'}, [{generate, line('$3'), '$2', '$4'}, '$6']}.

for_expr -> for open match_expr in bool_expr if bool_expr close fn_block :
    {lc, line('$1'), {block, line('$1'), '$9'}, [{generate, line('$4'), '$3', '$5'}, '$7']}.

% case expression
case_expr -> switch bool_expr case_body            : {'case', line('$1'), '$2', '$3'}.

case_body -> open_block case_patterns else fn_block endl close_block:
    '$2' ++ [{'clause', line('$3'), [{var, line('$3'), '_'}], [], '$4'}].
case_body -> open_block case_patterns endl close_block : '$2'.

case_patterns -> case_pattern case_patterns            : ['$1'|'$2'].
case_patterns -> case_pattern                          : ['$1'].
case_pattern -> case match_expr fn_block :
    {'clause', line('$1'), ['$2'], [], '$3'}.
case_pattern -> case match_expr when bool_expr fn_block :
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

literal -> range                : '$1'.
literal -> float                : '$1'.
literal -> bool_lit             : '$1'.
literal -> string               : {string,  line('$1'), unwrap('$1')}.
literal -> atom                 : '$1'.
literal -> var                  : '$1'.
literal -> open send_expr close : '$2'.
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
literal -> dotdotdot            : {dotdotdot, line('$1')}.
literal -> meta_block           : '$1'.
literal -> astify               : '$1'.
literal -> meta_astify          : '$1'.

bool_lit -> boolean             : {atom, line('$1'), unwrap('$1')}.

range -> signed_integer dotdot signed_integer :
    Line = line('$2'),
    Start = unwrap_signed_integer('$1'),
    Stop = unwrap_signed_integer('$3'),
    Args0 = ['$1', '$3'],

    Args = if
        Start > Stop ->
            Args0 ++ [{op, Line, '-', {integer, Line, 1}}];
        true ->
            Args0
    end,

    {call, Line, {remote, Line, {atom, Line, lists}, {atom, Line, seq}}, Args}.

range -> signed_integer : '$1'.
range -> bin_not integer: {op, line('$2'), op(unwrap('$1')), '$2'}.
range -> prefix_op float: {op, line('$2'), op(unwrap('$1')), '$2'}.
range -> add_op char: {op, line('$2'), op(unwrap('$1')), '$2'}.
range -> bin_not char: {op, line('$2'), op(unwrap('$1')), '$2'}.
range -> bool_not bool_lit: {op, line('$2'), op(unwrap('$1')), '$2'}.
range -> prefix_op var: {op, line('$2'), op(unwrap('$1')), '$2'}.
range -> prefix_op rec: {op, line('$2'), op(unwrap('$1')), '$2'}.
range -> prefix_op fun_call: {op, line('$2'), op(unwrap('$1')), '$2'}.
range -> prefix_op open send_expr close : {op, line('$2'), op(unwrap('$1')), '$3'}.

% list type
list -> open_list match_expr close_list : {cons, line('$1'), '$2', {nil, line('$1')}}.
list -> open_list close_list : {nil, line('$1')}.
list -> open_list match_expr list_items close_list : {cons, line('$1'), '$2', '$3'}.
list -> open_list match_expr split_op match_expr close_list : {cons, line('$1'), '$2', '$4'}.

list_items -> sep match_expr : {cons, line('$2'), '$2', {nil, line('$2')}}.
list_items -> sep match_expr split_op match_expr : {cons, line('$2'), '$2', '$4'}.
list_items -> sep match_expr list_items : {cons, line('$2'), '$2', '$3'}.

% tuple type
tuple -> open sep close    : {tuple, line('$1'), []}.
tuple -> open tuple_items close    : {tuple, line('$1'), '$2'}.

tuple_items -> match_expr sep : ['$1'].
tuple_items -> match_expr sep match_expr: ['$1', '$3'].
tuple_items -> match_expr sep tuple_items : ['$1'|'$3'].

% function call

fun_call -> atom fn_parameters           : {call, line('$1'), '$1', '$2'}.
fun_call -> var fn_parameters            : {call, line('$1'), '$1', '$2'}.
fun_call -> atom dot atom fn_parameters  :
    {call, line('$2'), {remote, line('$2'), '$1', '$3'}, '$4'}.
fun_call -> atom dot var fn_parameters  :
    {call, line('$2'), {remote, line('$2'), '$1', '$3'}, '$4'}.
fun_call -> var dot atom fn_parameters  :
    {call, line('$2'), {remote, line('$2'), '$1', '$3'}, '$4'}.
fun_call -> var dot var fn_parameters  :
    {call, line('$2'), {remote, line('$2'), '$1', '$3'}, '$4'}.
fun_call -> fun_call fn_parameters       : {call, line('$1'), '$1', '$2'}.

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

list_generator -> for bool_expr in bool_expr :
    Line = line('$1'),
    [{generate, Line, '$2', '$4'}].

list_generator -> for bool_expr in bool_expr if bool_expr :
    Line = line('$1'),
    [{generate, Line, '$2', '$4'},'$6'].

% records

rec -> atom dot var open_list atom close_list : {'record_field', line('$2'), '$3', unwrap('$1'), '$5'}.

rec_set -> atom dot var open_list close_list :
    {'record', line('$2'), '$3', unwrap('$1'), []}.

rec_set -> atom dot var open_list attr_sets close_list :
    {'record', line('$2'), '$3', unwrap('$1'), '$5'}.

rec_new -> atom open_list sep close_list :
    {'record', line('$2'), unwrap('$1'), []}.

rec_new -> atom open_list attr_sets close_list :
    {'record', line('$2'), unwrap('$1'), '$3'}.

attr_sets -> attr_set sep attr_sets : ['$1'|'$3'].
attr_sets -> atom sep attr_sets     : [{record_field, line('$1'), '$1'}|'$3'].
attr_sets -> attr_set               : ['$1'].

attr_set -> atom split_def_op add_expr:
    {typed_record_field,
          {record_field, line('$2'), '$1'},
          {type, line('$2'), union, [{atom, line('$2'), undefined}, fn_spec:convert_one('$3')]}}.

attr_set -> atom match bool_expr split_def_op add_expr:
    {typed_record_field,
          {record_field, line('$2'), '$1', '$3'},
          fn_spec:convert_one('$5')}.

attr_set  -> atom match bool_expr : {record_field, line('$2'), '$1', '$3'}.

% binary

binary -> open_bin binary_items close_bin : {'bin', line('$1'), '$2'}.
binary -> open_bin close_bin : {'bin', line('$1'), []}.

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

% used in @type

binary_item -> split_op mul_op integer:
    assert_atom('$2', '*'),
    Line = line('$1'),
    {bin_type_element, Line, {var, Line, '_'}, {var, Line, '_'}, '$3'}.

binary_item -> split_op integer:
    Line = line('$1'),
    {bin_type_element, Line, {var, Line, '_'}, '$2'}.

bin_type_def -> bin_type add_op bin_type_def :
    assert_atom('$2', '-'),
    ['$1'|'$3'].
bin_type_def -> bin_type : ['$1'].

bin_type -> atom split_op integer : {unwrap('$1'), unwrap('$3')}.
bin_type -> atom : unwrap('$1').

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line(T) when is_tuple(T) -> element(2, T);
line([H|_T]) -> element(2, H).

attr_level({attr,  _, _}) -> local;
attr_level({gattr, _, _}) -> global.

get_arity([{clause, _, Params, _, _}|_T]) -> length(Params).

op('%') -> 'rem';
op('//') -> 'div';
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

unwrap_signed_integer({op, _Line, '-', {integer, _Line, Value}}) ->
    -Value;
unwrap_signed_integer({op, _Line, '+', {integer, _Line, Value}}) ->
    Value;
unwrap_signed_integer({integer, _Line, Value}) ->
    Value.

run_attribute(Type, Attr, Line, Args) ->
    run_attribute(Type, Attr, attribute, Line, Args).

run_attribute(Type, Attr, Function, Line, Args) ->
    Result = try
        Attr:Function(Type, Line, Args)
    catch
        error:undef ->
            if
                Function == attribute ->
                    handle_undef_attribute(Type, Attr, Line, Args);
                true ->
                    fail(Line, "attribute handler not found", {Type, Attr, Function})
            end
    end,

    case Result of
        not_implemented ->
            fail(Line, "attribute not implemented", {Type, Attr});
        {error, Error} ->
            fail(Line, "error in attribute", {Type, Attr, Error});
        _ ->
            Result
    end.

handle_undef_attribute(local, Attr, Line, Args) ->
    {attribute, Line, Attr, normalise_args(Args)};

handle_undef_attribute(global, Attr, Line, Args) ->
    {global_attribute, Line, Attr, normalise_args(Args)}.

normalise_args([]) ->
    nil;
normalise_args([Arg]) ->
    erl_parse:normalise(Arg);
normalise_args({return, [Arg], Return}) ->
    {return, erl_parse:normalise(Arg), erl_parse:normalise(Return)};
normalise_args({return, Args, Return}) ->
    {return, [erl_parse:normalise(Arg) || Arg <- Args], erl_parse:normalise(Return)};
normalise_args(Args) ->
    [erl_parse:normalise(Arg) || Arg <- Args].
