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


syn keyword efeneCommentTodo      TODO FIXME XXX TBD contained
syn region efeneLineComment start="#" skip="\\$" end="$" keepend contains=efeneCommentTodo,@Spell

syn region  efeneStringD	       start=+"+  skip=+\\\\\|\\"+  end=+"\|$+ 

syn match   efeneSpecialCharacter "'\\.'"
syn match   efeneNumber	       "-\=\<\d\+L\=\>\|0[xob][0-9a-fA-F]\+\>"

syn keyword efeneConditional	if else for in receive after switch case when try catch object record
syn keyword efeneBoolean		true false

syn keyword	efeneFunction      public fn 
syn match	efeneBraces	   "[{}\[\]\.,]"
syn match	efeneParens	   "[()]"
syn match	efeneOperator	   "[\+\-\*/\|\^\&~=]"
syn keyword     efeneOperator	   and or andd orr xor not
syn match	efeneBraces	   "->"

syn sync fromstart
syn sync maxlines=100

if main_syntax == "efene"
  syn sync ccomment efeneComment
endif

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_efene_syn_inits")
  if version < 508
    let did_efene_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink efeneLineComment		Comment
  HiLink efeneCommentTodo		Todo
  HiLink efeneSpecial			Special
  HiLink efeneStringD			String
  HiLink efeneSpecialCharacter		efeneSpecial
  HiLink efeneNumber			Number	
  HiLink efeneConditional		Conditional
  HiLink efeneOperator			Operator
  HiLink efeneParens			Function
  HiLink efeneType			Type
  HiLink efeneStatement			Statement
  HiLink efeneFunction			Function
  HiLink efeneBraces			Function
  HiLink efeneError			Error
  HiLink javaScrParenError		efeneError
  HiLink efeneBoolean			Boolean

  HiLink efeneIdentifier		Identifier

  delcommand HiLink
endif

let b:current_syntax = "efene"
if main_syntax == 'efene'
  unlet main_syntax
endif

" vim: ts=8
