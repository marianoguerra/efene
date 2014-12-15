" Vim filetype plugin file
" Language:	efene
" Maintainer:	Mariano Guerra <mariano@marianoguerra.org>
" Last Change:	2014 Feb 09
" Last Change By Johannes: Wed, 21 Apr 2004 13:13:08 CEST

if exists("b:did_ftplugin") | finish | endif
let b:did_ftplugin = 1
let s:keepcpo= &cpo
set cpo&vim

setlocal cinkeys-=0#
setlocal indentkeys-=0#
setlocal includeexpr=substitute(v:fname,'\\.','/','g')
setlocal suffixesadd=.fn

set wildignore+=*.beam

setlocal expandtab shiftwidth=4 softtabstop=4 tabstop=8

let &cpo = s:keepcpo
unlet s:keepcpo
