Nonterminals
    expr_list grammar literal expressions expression function_def argument_def
    arguments block fun_expression obj_expression object_def fields
    function_call call_arguments call_argument call_params send_expr
    bool_and_expr bool_expr comp_expr add_expr mul_expr unary_expr patterns
    pattern list list_items tuple tuple_items binary binary_items binary_item
    binary_types concat_expr list_comp bin_comp list_generators bin_generators
    list_generator bin_generator try_expr recv_expr if_expr case_expr case_body
    case_patterns case_pattern catch_patterns catch_pattern if_patterns
    if_pattern block_expr bool_lit argument farity arrow_expr arrow_chains
    arrow_chain.

Terminals
    comp_op add_op mul_op bin_not bool_not match var open close fn sep
    open_list close_list open_block close_block open_bin close_bin integer
    float boolean endl atom string concat_op and_op xor_op or_op shift_op
    send_op split_op dot if when try catch receive after case for in
    bool_and_op bool_or_op object else char arrow.

Rootsymbol grammar.

Left 50 arrow.
Left 100 bool_or_op.
Left 200 bool_and_op.
Left 300 comp_op.
Right 400 concat_op.
Left 500 and_op.
Left 700 or_op.
Left 700 xor_op.
Left 700 add_op.
Left 800 shift_op.
Left 800 mul_op.
Left 900 bin_not.
Left 900 bool_not.
Left 1000 open.

grammar -> expr_list                            : '$1'.
% for eval
grammar -> expressions                          : '$1'.

expr_list -> endl expr_list                     : ['$2'].
expr_list -> fun_expression                     : ['$1'].
expr_list -> obj_expression                     : ['$1'].
expr_list -> fun_expression expr_list           : ['$1'|'$2'].
expr_list -> obj_expression expr_list           : ['$1'|'$2'].

fun_expression -> atom match function_def endl  : {fun_def, line('$1'), unwrap('$1'), '$3'}.

obj_expression -> atom match object_def endl    : {obj_def, line('$1'), unwrap('$1'), '$3'}.
object_def -> object open fields close          : '$3'.

fields -> atom fields                           : [unwrap('$1')|'$2'].
fields -> atom                                  : [unwrap('$1')].

expressions -> expression expressions           : ['$1'|'$2'].
expressions -> expression                       : ['$1'].

expression -> bool_expr match bool_expr endl    : {unwrap('$2'), line('$2'), '$1', '$3'}.
expression -> send_expr endl                    : '$1'.

send_expr -> bool_expr send_op send_expr        : {unwrap('$2'), line('$2'), '$1', '$3'}.
send_expr -> bool_expr : '$1'.

bool_expr -> bool_and_expr bool_or_op bool_expr : {unwrap('$2'), line('$2'), '$1', '$3'}.
bool_expr -> bool_and_expr                      : '$1'.

bool_and_expr -> comp_expr bool_and_op bool_and_expr : {unwrap('$2'), line('$2'), '$1', '$3'}.
bool_and_expr -> comp_expr                           : '$1'.

comp_expr -> concat_expr comp_op comp_expr      : {unwrap('$2'), line('$2'), '$1', '$3'}.
comp_expr -> concat_expr                        : '$1'.

concat_expr -> add_expr concat_op concat_expr   : {unwrap('$2'), line('$2'), '$1', '$3'}.
concat_expr -> add_expr                         : '$1'.

add_expr -> mul_expr add_op add_expr    : {unwrap('$2'), line('$2'), '$1', '$3'}.
add_expr -> mul_expr or_op add_expr     : {unwrap('$2'), line('$2'), '$1', '$3'}.
add_expr -> mul_expr xor_op add_expr    : {unwrap('$2'), line('$2'), '$1', '$3'}.
add_expr -> mul_expr                    : '$1'.

mul_expr -> unary_expr mul_op mul_expr  : {unwrap('$2'), line('$2'), '$1', '$3'}.
mul_expr -> unary_expr and_op mul_expr  : {unwrap('$2'), line('$2'), '$1', '$3'}.
mul_expr -> unary_expr                  : '$1'.
mul_expr -> unary_expr shift_op mul_expr: {unwrap('$2'), line('$2'), '$1', '$3'}.

unary_expr -> bool_not bool_lit                : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> bool_not var                     : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> bool_not function_call           : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> bool_not open bool_expr close    : {unwrap('$1'), line('$1'), '$3'}.
unary_expr -> add_op integer                   : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> add_op float                     : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> add_op var                       : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> add_op open bool_expr close      : {unwrap('$1'), line('$1'), '$3'}.
unary_expr -> add_op function_call             : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> bin_not integer                  : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> bin_not var                      : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> bin_not open bool_expr close     : {unwrap('$1'), line('$1'), '$3'}.
unary_expr -> bin_not function_call            : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> block_expr                       : '$1'.

block_expr -> try_expr          : '$1'.
block_expr -> if_expr           : '$1'.
block_expr -> case_expr         : '$1'.
block_expr -> recv_expr         : '$1'.
block_expr -> function_def      : '$1'.
block_expr -> arrow_expr        : '$1'.

literal -> integer              : '$1'.
literal -> float                : '$1'.
literal -> bool_lit             : '$1'.
literal -> string               : {string,  line('$1'), unwrap('$1')}.
literal -> list                 : '$1'.
literal -> tuple                : '$1'.
literal -> binary               : '$1'.
literal -> var                  : '$1'.
literal -> atom                 : '$1'.
literal -> open bool_expr close : '$2'.
literal -> function_call        : '$1'.
literal -> list_comp            : '$1'.
literal -> bin_comp             : '$1'.
literal -> char                 : '$1'.
literal -> farity               : '$1'.

arrow_expr  -> literal arrow_chains: {arrow_call, line('$1'), '$1', '$2'}.
arrow_expr  -> literal : '$1'.

arrow_chains -> arrow_chain arrow_chains : ['$1'|'$2'].
arrow_chains -> arrow_chain : ['$1'].
arrow_chain -> arrow atom call_params: {line('$1'), '$2', '$3'}.
arrow_chain -> arrow atom dot atom call_params: {line('$1'), '$2', '$4', '$5'}.
arrow_chain -> arrow var dot var call_params: {line('$1'), '$2', '$4', '$5'}.
arrow_chain -> arrow var dot atom call_params: {line('$1'), '$2', '$4', '$5'}.
arrow_chain -> arrow atom dot var call_params: {line('$1'), '$2', '$4', '$5'}.

farity -> fn atom mul_op integer   :
    case unwrap('$3') == '/' of
        true -> {'fun', line('$1'), {function, unwrap('$2'), unwrap('$4')}};
        false -> throw({error, {unwrap('$3'), yecc, "'/' expected on function artity expression **"}})
    end.

farity -> fn atom dot atom mul_op integer   :
    case unwrap('$5') == '/' of
        true -> {'fun', line('$1'), {function, unwrap('$2'), unwrap('$4'), unwrap('$6')}};
        false -> throw({error, {unwrap('$5'), yecc, "'/' expected on function artity expression **"}})
    end.

bool_lit -> boolean                : {atom, line('$1'), unwrap('$1')}.


function_call -> var call_params            : {call,    line('$1'), ['$1'], '$2'}.
function_call -> atom call_params           : {call,    line('$1'), ['$1'], '$2'}.
function_call -> atom dot atom call_params  : {call,    line('$2'), ['$1', '$3'], '$4'}.
function_call -> var dot atom call_params   : {call,    line('$2'), ['$1', '$3'], '$4'}.
function_call -> var dot var call_params    : {call,    line('$2'), ['$1', '$3'], '$4'}.
function_call -> atom dot var call_params   : {call,    line('$2'), ['$1', '$3'], '$4'}.
function_call -> function_call call_params  : {call,    line('$1'), ['$1'], '$2'}.

call_params -> open call_arguments close    : lists:flatten('$2').
call_params -> open close                   : [].

call_arguments -> call_argument                     : ['$1'].
call_arguments -> call_argument sep call_arguments  : ['$1'|'$3'].
call_arguments -> call_argument call_arguments      : ['$1'|'$2'].

call_argument -> bool_expr : '$1'.

function_def -> fn patterns : {unwrap('$1'), line('$1'), '$2'}.
function_def -> block       : {'fn', line('$1'), [{pattern,{'(',line('$1'),[]},[],'$1'}]}.

patterns -> pattern patterns                    : ['$1'|'$2'].
patterns -> pattern                             : ['$1'].
pattern  -> argument_def block                  : {pattern, '$1', [], '$2'}.
pattern  -> argument_def when bool_expr block   : {pattern, '$1', ['$3'], '$4'}.

argument_def -> open arguments close  : {unwrap('$1'), line('$1'), lists:flatten('$2')}.
argument_def -> open close            : {unwrap('$1'), line('$1'), []}.

arguments -> argument               : ['$1'].
arguments -> argument sep arguments : ['$1'|'$3'].
arguments -> argument arguments     : ['$1'|'$2'].

argument -> unary_expr                : '$1'.
argument -> unary_expr match var      : {unwrap('$2'), line('$2'), '$1', '$3'}.

block -> open_block expressions close_block   : {unwrap('$1'), line('$1'), '$2'}.
block -> open_block send_expr close_block     : {unwrap('$1'), line('$1'), ['$2']}.

list -> open_list close_list            : {nil, line('$1')}.
list -> open_list list_items close_list : '$2'.

list_items -> bool_expr sep list_items      : {cons, line('$1'), '$1', '$3'}.
list_items -> bool_expr list_items          : {cons, line('$1'), '$1', '$2'}.
list_items -> bool_expr                     : {cons, line('$1'), '$1', {nil, line('$1')}}.
list_items -> bool_expr split_op bool_expr  : {cons, line('$1'), '$1', '$3'}.

tuple -> open sep close             : {tuple, line('$1'), []}.
tuple -> open tuple_items close     : {tuple, line('$1'), '$2'}.

tuple_items -> bool_expr sep tuple_items    : ['$1'|'$3'].
tuple_items -> bool_expr tuple_items        : ['$1'|'$2'].
tuple_items -> bool_expr sep bool_expr      : ['$1','$3'].
tuple_items -> bool_expr bool_expr          : ['$1','$2'].
tuple_items -> bool_expr sep                : ['$1'].

binary -> open_bin binary_items close_bin   : {bin, line('$1'), lists:flatten('$2')}.

binary_items -> binary_item sep binary_items    : ['$1'|'$3'].
binary_items -> binary_item                     : ['$1'].

binary_item -> bool_expr                                 : {bin_element, line('$1'), '$1', default, default}.
binary_item -> bool_expr split_op bool_expr              : {bin_element, line('$1'), '$1', '$3', default}.
binary_item -> bool_expr split_op bool_expr binary_types : {bin_element, line('$1'), '$1', '$3', '$4'}.
binary_item -> bool_expr binary_types                    : {bin_element, line('$1'), '$1', default, '$2'}.

binary_types -> atom binary_types : [unwrap('$1')|'$2'].
binary_types -> atom              : [unwrap('$1')].

list_comp -> open_list bool_expr list_generators close_list : {lc, line('$1'), '$2', lists:flatten('$3')}.

list_generators -> list_generator list_generators   : ['$1'|'$2'].
list_generators -> list_generator                   : '$1'.

list_generator -> for bool_expr in bool_expr              : [{generate, line('$1'), '$2', '$4'}].
list_generator -> for bool_expr in bool_expr if bool_expr : [{generate, line('$1'), '$2', '$4'},'$6'].

%% XXX: I don't know the difference between a list comprehension with binary generator and a binary comprehension
%% this might change in the future
bin_comp -> open_bin bool_expr bin_generators close_bin : {lc, line('$1'), '$2', lists:flatten('$3')}.

bin_generators -> bin_generator bin_generators   : ['$1'|'$2'].
bin_generators -> bin_generator                  : '$1'.

bin_generator -> for bool_expr in bool_expr              : [{b_generate, line('$1'), '$2', '$4'}].
bin_generator -> for bool_expr in bool_expr if bool_expr : [{b_generate, line('$1'), '$2', '$4'},'$6'].

try_expr -> try block                                       : {'try', line('$1'), '$2'}.
try_expr -> try block catch catch_patterns                  : {'try', line('$1'), '$2', '$4'}.
try_expr -> try block catch catch_patterns else block       : {'try', line('$1'), '$2', '$4', '$6'}.

if_expr  -> if if_patterns                       : {'if', line('$1'), '$2'}.
if_expr  -> if if_patterns else block            : {'if', line('$1'), '$2', '$4'}.

case_expr -> case bool_expr case_body            : {'case', line('$1'), '$2', '$3'}.
case_expr -> case bool_expr case_body else block : {'case', line('$1'), '$2', '$3', '$5'}.

case_body -> open_block case_patterns endl close_block : '$2'.

case_patterns -> case_pattern case_patterns            : ['$1'|'$2'].
case_patterns -> case_pattern                          : ['$1'].
case_pattern -> bool_expr block                        : {'clause', line('$1'), ['$1'], [], '$2'}.
case_pattern -> bool_expr when bool_expr block         : {'clause', line('$1'), ['$1'], ['$3'], '$4'}.

if_patterns -> if_pattern if_patterns                  : ['$1'|'$2'].
if_patterns -> if_pattern                              : ['$1'].
if_pattern -> bool_expr block                          : {pattern, nil, {'(', line('$1'), ['$1']}, '$2'}.

catch_patterns -> catch_pattern catch_patterns         : ['$1'|'$2'].
catch_patterns -> catch_pattern                        : ['$1'].
catch_pattern -> open atom literal close block         :
    AtomName = unwrap('$2'),
    case AtomName == 'throw' orelse AtomName == 'error' orelse AtomName == 'exit' of
        true ->
            {pattern, {'(', line('$1'), [{tuple, line('$1'), ['$2', '$3', {var, line('$1'), '_'}]}]}, [], '$5'};
        false ->
            throw({error, {AtomName, yecc, "'throw', 'error' or 'exit' expected on catch **"}})
    end.
catch_pattern -> atom literal block                    :
    AtomName = unwrap('$1'),
    case AtomName == 'throw' orelse AtomName == 'error' orelse AtomName == 'exit' of
        true ->
            {pattern, {'(', line('$1'), [{tuple, line('$1'), ['$1', '$2', {var, line('$1'), '_'}]}]}, [], '$3'};
        false ->
            throw({error, {AtomName, yecc, "'throw', 'error' or 'exit' expected on catch **"}})
    end.
catch_pattern -> var literal block         :
    {pattern, {'(', line('$1'), [{tuple, line('$1'), ['$1', '$2', {var, line('$1'), '_'}]}]}, [], '$3'}.
catch_pattern -> open var literal close block         :
    {pattern, {'(', line('$1'), [{tuple, line('$1'), ['$2', '$3', {var, line('$1'), '_'}]}]}, [], '$5'}.
catch_pattern -> open literal close block:
    {pattern, {'(', line('$1'), [{tuple, line('$1'), [{atom, line('$1'), throw}, '$2', {var, line('$1'), '_'}]}]}, [], '$4'}.
catch_pattern -> literal block:
    {pattern, {'(', line('$1'), [{tuple, line('$1'), [{atom, line('$1'), throw}, '$1', {var, line('$1'), '_'}]}]}, [], '$2'}.

recv_expr -> receive case_patterns                          : {'receive', line('$1'), '$2'}.
recv_expr -> receive case_patterns after literal block      : {'receive', line('$1'), '$2', '$4', '$5'}.

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line({_, Line})         -> Line;
line({_, Line, _})      -> Line;
line({_, Line, _, _})   -> Line.

