Nonterminals 
    expr_list grammar literal expressions expression function_def argument_def
    arguments block fun_expression function_call call_arguments call_argument
    call_params send_expr bool_and_expr bool_expr comp_expr add_expr
    mul_expr unary_expr patterns pattern list list_items tuple tuple_items
    binary binary_items binary_item binary_types concat_expr list_comp bin_comp
    list_generators bin_generators list_generator bin_generator try_expr
    recv_expr catch_patterns catch_pattern.

Terminals 
    comp_op add_op mul_op unary_op match var open close fn sep open_list
    close_list open_block close_block open_bin close_bin integer float boolean
    endl atom string concat_op and_op xor_op or_op shift_op send_op
    split_op dot if try catch finally receive after bool_and_op bool_or_op.

Rootsymbol grammar.

Left 100 bool_or_op.
Left 200 bool_and_op.
Left 300 comp_op.
Right 400 concat_op.
Left 600 and_op.
Left 700 or_op.
Left 700 xor_op.
Left 700 add_op.
Left 700 shift_op.
Left 800 mul_op.
Left 900 unary_op.
Left 1000 open.

grammar -> expr_list                            : '$1'.

expr_list -> fun_expression                     : ['$1'].
expr_list -> fun_expression expr_list           : ['$1'|'$2'].

fun_expression -> atom match function_def endl  : {fun_def, line('$1'), unwrap('$1'), '$3'}.

expressions -> expression                       : ['$1'].
expressions -> expression expressions           : ['$1'|'$2'].

expression -> bool_expr match bool_expr endl    : {unwrap('$2'), line('$2'), '$1', '$3'}.
expression -> send_expr endl                    : '$1'.

send_expr -> bool_expr send_op send_expr        : {unwrap('$2'), line('$2'), '$1', '$3'}.
send_expr -> bool_expr : '$1'.

bool_expr -> bool_and_expr bool_or_op bool_expr      : {unwrap('$2'), line('$2'), '$1', '$3'}.
bool_expr -> bool_and_expr                           : '$1'.

bool_and_expr -> comp_expr bool_and_op bool_and_expr : {unwrap('$2'), line('$2'), '$1', '$3'}.
bool_and_expr -> comp_expr                           : '$1'.

comp_expr -> concat_expr comp_op comp_expr      : {unwrap('$2'), line('$2'), '$1', '$3'}.
comp_expr -> concat_expr                        : '$1'.

concat_expr -> add_expr concat_op concat_expr    : {unwrap('$2'), line('$2'), '$1', '$3'}.
concat_expr -> add_expr                          : '$1'.

add_expr -> mul_expr add_op add_expr    : {unwrap('$2'), line('$2'), '$1', '$3'}.
add_expr -> mul_expr shift_op add_expr  : {unwrap('$2'), line('$2'), '$1', '$3'}.
add_expr -> mul_expr or_op add_expr     : {unwrap('$2'), line('$2'), '$1', '$3'}.
add_expr -> mul_expr xor_op add_expr    : {unwrap('$2'), line('$2'), '$1', '$3'}.
add_expr -> mul_expr                    : '$1'.

mul_expr -> unary_expr mul_op mul_expr  : {unwrap('$2'), line('$2'), '$1', '$3'}.
mul_expr -> unary_expr and_op mul_expr  : {unwrap('$2'), line('$2'), '$1', '$3'}.
mul_expr -> unary_expr                  : '$1'.

unary_expr -> unary_op literal  : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> add_op literal    : {unwrap('$1'), line('$1'), '$2'}.
unary_expr -> literal           : '$1'.

literal -> integer              : {integer, line('$1'), unwrap('$1')}.
literal -> float                : {float,   line('$1'), unwrap('$1')}.
literal -> boolean              : {atom,    line('$1'), unwrap('$1')}.
literal -> string               : {string,  line('$1'), unwrap('$1')}.
literal -> list                 : '$1'.
literal -> tuple                : '$1'.
literal -> binary               : '$1'.
literal -> var                  : {var,     line('$1'), unwrap('$1')}.
literal -> atom                 : {atom,    line('$1'), unwrap('$1')}.
literal -> open bool_expr close : '$2'.
literal -> function_call        : '$1'.
literal -> function_def         : '$1'.
literal -> list_comp            : '$1'.
literal -> bin_comp            : '$1'.
literal -> try_expr                 : '$1'.
literal -> recv_expr                 : '$1'.

function_call -> var call_params            : {call,        line('$1'), '$1', '$2'}.
function_call -> atom call_params           : {callatom,    line('$1'), ['$1'], '$2'}.
function_call -> atom dot atom call_params  : {callatom,    line('$2'), ['$1', '$3'], '$4'}.
function_call -> function_call call_params  : {call,        line('$1'), '$1', '$2'}.

call_params -> open call_arguments close            : lists:flatten('$2').
call_params -> open close                           : [].

call_arguments -> call_argument                     : ['$1'].
call_arguments -> call_argument sep call_arguments  : ['$1'|'$3'].
call_arguments -> call_argument call_arguments      : ['$1'|'$2'].

call_argument -> bool_expr          : '$1'.

function_def -> fn patterns     : {unwrap('$1'), line('$1'), '$2'}.
function_def -> block           : {'fn', line('$1'), [{pattern,{'(',line('$1'),[]},[],'$1'}]}.

patterns -> pattern patterns     : ['$1'|'$2'].
patterns -> pattern              : ['$1'].
pattern  -> argument_def block   : {pattern, '$1', [], '$2'}.
pattern  -> argument_def if bool_expr block   : {pattern, '$1', ['$3'], '$4'}.

argument_def -> open arguments close : {unwrap('$1'), line('$1'), lists:flatten('$2')}.
argument_def -> open close           : {unwrap('$1'), line('$1'), []}.

arguments -> unary_expr               : ['$1'].
arguments -> unary_expr sep arguments : ['$1'|'$3'].
arguments -> unary_expr arguments     : ['$1'|'$2'].

block -> open_block expressions close_block   : {unwrap('$1'),    line('$1'), '$2'}.

list -> open_list close_list            : {nil, line('$1')}.
list -> open_list list_items close_list : '$2'.

list_items -> bool_expr sep list_items      : {cons, line('$1'), '$1', '$3'}.
list_items -> bool_expr                     : {cons, line('$1'), '$1', {nil, line('$1')}}.
list_items -> bool_expr split_op bool_expr  : {cons, line('$1'), '$1', '$3'}.
list_items -> bool_expr sep                 : {cons, line('$1'), '$1', {nil, line('$1')}}.

tuple -> open sep close             : {tuple, line('$1'), []}.
tuple -> open tuple_items close     : {tuple, line('$1'), '$2'}.

tuple_items -> bool_expr sep tuple_items    : ['$1'|'$3'].
tuple_items -> bool_expr sep bool_expr      : ['$1','$3'].
tuple_items -> bool_expr sep                : ['$1'].

binary -> open_bin binary_items close_bin   : {bin, line('$1'), lists:flatten('$2')}.

binary_items -> binary_item sep binary_items    : ['$1'|'$3'].
binary_items -> binary_item                     : ['$1'].

binary_item -> bool_expr                           : {bin_element, line('$1'), '$1', default, default}.
binary_item -> bool_expr split_op bool_expr        : {bin_element, line('$1'), '$1', '$3', default}.
binary_item -> bool_expr split_op bool_expr binary_types : {bin_element, line('$1'), '$1', '$3', '$4'}.
binary_item -> bool_expr binary_types : {bin_element, line('$1'), '$1', default, '$2'}.

binary_types -> atom binary_types : [unwrap('$1')|'$2'].
binary_types -> atom : [unwrap('$1')].

list_comp -> open_list bool_expr list_generators close_list : {lc, line('$1'), '$2', lists:flatten('$3')}.

list_generators -> list_generator list_generators   : ['$1'|'$2'].
list_generators -> list_generator                   : '$1'.

list_generator -> atom bool_expr atom bool_expr  : [{generate, line('$1'), '$2', '$4'}]. 
list_generator -> atom bool_expr atom bool_expr if bool_expr : [{generate, line('$1'), '$2', '$4'},'$6']. 

%% XXX: I don't know the difference between a list comprehension with binary generator and a binary comprehension
%% this might change in the future
bin_comp -> open_bin bool_expr bin_generators close_bin : {lc, line('$1'), '$2', lists:flatten('$3')}.

bin_generators -> bin_generator bin_generators   : ['$1'|'$2'].
bin_generators -> bin_generator                  : '$1'.

bin_generator -> atom bool_expr atom bool_expr  : [{b_generate, line('$1'), '$2', '$4'}]. 
bin_generator -> atom bool_expr atom bool_expr if bool_expr : [{b_generate, line('$1'), '$2', '$4'},'$6']. 

try_expr -> try block                                           : {trys, line('$1'), '$2'}.
try_expr -> try block catch catch_patterns                      : {trys, line('$1'), '$2', '$4'}.
try_expr -> try block catch catch_patterns finally block        : {trys, line('$1'), '$2', '$4', '$6'}.

catch_patterns -> catch_pattern catch_patterns             : ['$1'|'$2'].
catch_patterns -> catch_pattern                            : ['$1'].
%% TODO: restrict atom to throw, error and exit
catch_pattern -> open atom literal close block             : {pattern, {unwrap('$1'), line('$1'), [{tuple, line('$1'), ['$2', '$3', {var, line('$1'), '_'}]}]}, [], '$5'}.
catch_pattern -> open literal close block                  : {pattern, {unwrap('$1'), line('$1'), [{tuple, line('$1'), [{atom, line('$1'), throw}, '$2', {var, line('$1'), '_'}]}]}, [], '$4'}.

%% {tuple,35,[{atom,35,throw},{integer,35,2},{var,35,'_'}]}
%% {pattern, {'(',35,[{tuple,35,[{atom,35,throw},{integer,35,2},{var,35,'_'}]}]}

recv_expr -> receive patterns                            : {receives, line('$1'), '$2'}.
recv_expr -> receive patterns after literal block        : {receives, line('$1'), '$2', '$4', '$5'}.

Erlang code.

unwrap({_,V})   -> V;
unwrap({_,_,V}) -> V.

line({_, Line})         -> Line;
line({_, Line, _})      -> Line;
line({_, Line, _, _})   -> Line.

