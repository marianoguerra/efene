" Vim syntax file
" Language:	efene
" Maintainer:	Mariano Guerra <luismarianoguerra@gmail.com>
" URL:		http://www.efene.org.ar
" Last Change:	2009 Oct 24

if !exists("main_syntax")
  if version < 600
    syntax clear
  elseif exists("b:current_syntax")
    finish
  endif
  let main_syntax = 'efene'
endif

" Drop fold if it set but vim doesn't support it.
if version < 600 && exists("efene_fold")
  unlet efene_fold
endif

syn case ignore

syn keyword efeneStatement	false true
syn keyword efeneFunction	fn begin
syn keyword efeneConditional	else if receive after case switch when
syn keyword efeneRepeat		for in
syn keyword efeneOperator	and in not or xor orr andd
syn keyword efeneException	try catch

syn match   efeneComment	"#.*$" contains=efeneTodo,@Spell
syn keyword efeneTodo		FIXME NOTE NOTES TODO XXX contained

syn match efeneFunction	"^@@\=\a\+"

syn match   efeneNumber	"\<0[oO]\=\o\+[Ll]\=\>"
syn match   efeneNumber	"\<0[xX]\x\+[Ll]\=\>"
syn match   efeneNumber	"\<0[bB][01]\+[Ll]\=\>"
syn match   efeneNumber	"\<\%([1-9]\d*\|0\)[Ll]\=\>"
syn match   efeneNumber	"\<\d\+[jJ]\>"
syn match   efeneNumber	"\<\d\+[eE][+-]\=\d\+[jJ]\=\>"
syn match   efeneNumber
\ "\<\d\+\.\%([eE][+-]\=\d\+\)\=[jJ]\=\%(\W\|$\)\@="
syn match   efeneNumber
\ "\%(^\|\W\)\@<=\d*\.\d\+\%([eE][+-]\=\d\+\)\=[jJ]\=\>"
syn region  efeneStringD	       start=+"+  skip=+\n\\\\\|\\"+  end=+"+ 

syn keyword efeneBuiltin	abs throw
syn match   efeneSpaceError	display "\t"

syn match   efeneSpecialCharacter "'\\.'"

syn match	efeneBraces	   "[{}\[\]\.,]"
syn match	efeneParens	   "[()]"
syn match	efeneOperator	   "[\$\+\-\*/\|\^\&~=]"
syn match	efeneSep	   ":"
syn match	efeneSep	   "::"
syn match	efeneSep	   "->"

syn sync fromstart
syn sync maxlines=100

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
command -nargs=+ HiLink hi def link <args>
" The default highlight links.  Can be overridden later.
HiLink efeneStatement	Statement
HiLink efeneConditional	Conditional
HiLink efeneRepeat		Repeat
HiLink efeneOperator		Operator
HiLink efeneSep		Operator
HiLink efeneException	Exception
HiLink efeneComment		Comment
HiLink efeneTodo		Todo
HiLink efeneNumber		Number
HiLink efeneBuiltin	Function
HiLink efeneSpaceError	Error
HiLink efeneStringD			String
HiLink efeneSpecialCharacter		efeneSpecial
HiLink efeneSpecialCharacter		efeneSpecial
HiLink efeneParens			Function
HiLink efeneBraces			Function
HiLink efeneFunction			Function

delcommand HiLink

let b:current_syntax = "efene"
if main_syntax == 'efene'
  unlet main_syntax
endif

" vim: ts=8
