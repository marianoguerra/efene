" Vim syntax file
" Language:     Efene
" Maintainer:   Mariano Guerra <mariano@marianoguerra.org>
" Last Update:  2013-Jul-25
" License:      Vim license

" Acknowledgements: This script is an adaptation from erlang.vim
"
if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif

let s:cpo_save = &cpo
set cpo&vim

" Case sensitive
syn case match

if version >= 600
  setlocal iskeyword+=$,@-@
endif

" Comments
syn match efeneShebang           '^#!.*'

" Numbers (minimum base is 2, maximum is 36.)
syn match efeneNumberInteger '\<\d\+\>'
syn match efeneNumberInteger '\<\%([2-9]\|[12]\d\|3[0-6]\)\+#[[:alnum:]]\+\>'
syn match efeneNumberFloat   '\<\d\+\.\d\+\%([eE][+-]\=\d\+\)\=\>'

" Strings, atoms, characters
syn region efeneString            start=/"/ end=/"/ contains=efeneStringModifier
syn region efeneQuotedAtom        start=/'/ end=/'/ contains=efeneQuotedAtomModifier
syn match efeneStringModifier     '\~\a\|\\\%(\o\{1,3}\|x\x\x\|x{\x\+}\|\^.\|.\)' contained
syn match efeneQuotedAtomModifier '\~\a\|\\\%(\o\{1,3}\|x\x\x\|x{\x\+}\|\^.\|.\)' contained
syn match efeneModifier           '\$\%([^\\]\|\\\%(\o\{1,3}\|x\x\x\|x{\x\+}\|\^.\|.\)\)'

" Operators, separators
syn match efeneOperator   '==\|===\|!=\|!==\|<\|<=\|>\|>=\|++\|--\|=\|!\|<-\|+\|-\|\*\|\/|\/\/|%|->|->>|<<-'
syn match efeneOperator   '<<\|>>\|&\|^\|\~'
syn keyword efeneOperator or xor orr and andd not
syn match efeneBracket    '{\|}\|\[\|]'
syn match efenePipe       '::'

" Atoms, function calls (order is important)
syn match efeneAtom           '\<\l[[:alnum:]_@]*' contains=efeneBoolean
syn keyword efeneBoolean      true false contained
syn match efeneLocalFuncCall  '\<\a[[:alnum:]_@]*\>\%(\%(\s\|\n\|%.*\n\)*(\)\@=' contains=efeneBIF
syn match efeneLocalFuncRef   '\<\a[[:alnum:]_@]*\>\%(\%(\s\|\n\|%.*\n\)*/\)\@='
syn match efeneGlobalFuncCall '\<\%(\a[[:alnum:]_@]*\%(\s\|\n\|%.*\n\)*\.\%(\s\|\n\|%.*\n\)*\)*\a[[:alnum:]_@]*\%(\s\|\n\|%.*\n\)*\.\%(\s\|\n\|%.*\n\)*\a[[:alnum:]_@]*\>\%(\%(\s\|\n\|%.*\n\)*(\)\@='
syn match efeneGlobalFuncRef  '\<\%(\a[[:alnum:]_@]*\%(\s\|\n\|%.*\n\)*\.\%(\s\|\n\|%.*\n\)*\)*\a[[:alnum:]_@]*\%(\s\|\n\|%.*\n\)*\.\%(\s\|\n\|%.*\n\)*\a[[:alnum:]_@]*\>\%(\%(\s\|\n\|%.*\n\)*/\)\@='

" Variables, macros, records
syn match efeneVariable '\<[A-Z_][[:alnum:]_@]*'
syn match efeneRecord   '#r\.\l[[:alnum:]_@]*'

" Bitstrings
syn match efeneBitType '\%(\/\%(\s\|\n\|%.*\n\)*\)\@<=\%(integer\|float\|binary\|bytes\|bitstring\|bits\|binary\|utf8\|utf16\|utf32\|signed\|unsigned\|big\|little\|native\|unit\)\%(\%(\s\|\n\|%.*\n\)*-\%(\s\|\n\|%.*\n\)*\%(integer\|float\|binary\|bytes\|bitstring\|bits\|binary\|utf8\|utf16\|utf32\|signed\|unsigned\|big\|little\|native\|unit\)\)*'

" Constants and Directives
syn match efeneUnknownAttribute '^\s*@\%(\s\|\n\|%.*\n\)*\l[[:alnum:]_@]*'
syn match efeneAttribute '^\s*@\%(\s\|\n\|%.*\n\)*\%(behaviou\=r\|compile\|export\(_type\)\=\|file\|import\|module\|author\|copyright\|doc\|vsn\|on_load\)\>'
syn match efeneInclude   '^\s*@\%(\s\|\n\|%.*\n\)*\%(include\|include_lib\)\>'
syn match efeneRecordDef '^\s*@\%(\s\|\n\|%.*\n\)*record\>'
syn match efeneDefine    '^\s*@\%(\s\|\n\|%.*\n\)*\%(define\|undef\)\>'
syn match efenePreCondit '^\s*@\%(\s\|\n\|%.*\n\)*\%(ifdef\|ifndef\|else\|endif\)\>'
syn match efeneType      '^\s*@\%(\s\|\n\|%.*\n\)*\%(spec\|type\|opaque\|callback\)\>'

" Keywords
syn keyword efeneKeyword after begin case catch end fn if for in match else
syn keyword efeneKeyword receive when try

" Build-in-functions (BIFs)
syn keyword efeneBIF abs alive apply atom_to_binary atom_to_list contained
syn keyword efeneBIF binary_part binary_to_atom contained
syn keyword efeneBIF binary_to_existing_atom binary_to_float contained
syn keyword efeneBIF binary_to_integer bitstring_to_list contained
syn keyword efeneBIF binary_to_list binary_to_term bit_size contained
syn keyword efeneBIF byte_size check_old_code check_process_code contained
syn keyword efeneBIF concat_binary date delete_module demonitor contained
syn keyword efeneBIF disconnect_node element erase error exit contained
syn keyword efeneBIF float float_to_binary float_to_list contained
syn keyword efeneBIF garbage_collect get get_keys group_leader contained
syn keyword efeneBIF halt hd integer_to_binary integer_to_list contained
syn keyword efeneBIF iolist_to_binary iolist_size is_alive contained
syn keyword efeneBIF is_atom is_binary is_bitstring is_boolean contained
syn keyword efeneBIF is_float is_function is_integer is_list contained
syn keyword efeneBIF is_number is_pid is_port is_process_alive contained
syn keyword efeneBIF is_record is_reference is_tuple length link contained
syn keyword efeneBIF list_to_atom list_to_binary contained
syn keyword efeneBIF list_to_bitstring list_to_existing_atom contained
syn keyword efeneBIF list_to_float list_to_integer list_to_pid contained
syn keyword efeneBIF list_to_tuple load_module make_ref max min contained
syn keyword efeneBIF module_loaded monitor monitor_node node contained
syn keyword efeneBIF nodes now open_port pid_to_list port_close contained
syn keyword efeneBIF port_command port_connect pre_loaded contained
syn keyword efeneBIF process_flag process_flag process_info contained
syn keyword efeneBIF process purge_module put register registered contained
syn keyword efeneBIF round self setelement size spawn spawn_link contained
syn keyword efeneBIF spawn_monitor spawn_opt split_binary contained
syn keyword efeneBIF statistics term_to_binary throw time tl contained
syn keyword efeneBIF trunc tuple_size tuple_to_list unlink contained
syn keyword efeneBIF unregister whereis contained

" Sync at the beginning of functions: if this is not used, multiline string
" are not always recognized, and the indentation script cannot use the
" "searchpair" (because it would not always skip strings and comments when
" looking for keywords and opening parens/brackets).
syn sync match efeneSync grouphere NONE "^[a-z]\s*("
let b:efene_syntax_synced = 1

" Define the default highlighting. See ":help group-name" for the groups and
" their colors.

" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_efene_inits")
  if version < 508
    let did_efene_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink efeneShebang Comment

  " Numbers
  HiLink efeneNumberInteger Number
  HiLink efeneNumberFloat Float

  " Strings, atoms, characters
  HiLink efeneString String

  HiLink efeneQuotedAtom String

  HiLink efeneStringModifier Special
  HiLink efeneQuotedAtomModifier Special
  HiLink efeneModifier Special

  " Operators, separators
  HiLink efeneOperator Operator
  HiLink efeneBracket Delimiter
  HiLink efenePipe Delimiter

  " Atoms, functions, variables, macros
  "HiLink efeneAtom String
  HiLink efeneLocalFuncCall Normal
  HiLink efeneLocalFuncRef Normal
  HiLink efeneGlobalFuncCall Normal
  HiLink efeneGlobalFuncRef Normal
  HiLink efeneVariable Identifier
  HiLink efeneRecord Structure

  " Bitstrings
  " Constants and Directives
  HiLink efeneAttribute Keyword
  HiLink efeneMacroDef Macro
  HiLink efeneUnknownAttribute Normal
  HiLink efeneInclude Include
  HiLink efeneRecordDef Keyword
  HiLink efeneDefine Define
  HiLink efenePreCondit PreCondit
  HiLink efeneType Type

  " Keywords
  HiLink efeneKeyword Keyword

  " Build-in-functions (BIFs)
  HiLink efeneBIF Function

  HiLink efeneBoolean Boolean
  HiLink efeneExtra Statement
  HiLink efeneSignal Statement

  delcommand HiLink
endif

let b:current_syntax = "efene"

let &cpo = s:cpo_save
unlet s:cpo_save

" vim: sw=2 et
