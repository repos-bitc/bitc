" Vim filetype plugin file
" Language:     BitC
" Maintainer:   Jonathan Shapiro <shap at eros-os org>
" Last Changed: 24 Sep 2010

if exists("b:did_ftplugin")
  finish
endif

let b:did_ftplugin = 1

let b:undo_ftplugin = "setl fo<"

" Set 'formatoptions' to break comment lines but not other lines,
" and insert the comment leader when hitting <CR> or using "o".
setlocal fo-=t fo+=croql

setlocal comments=sO:*\ -,mO:*\ \ ,exO:*/,s1:/*,mb:*,ex:*/,://

" When the matchit plugin is loaded, this makes the % command skip parens and
" braces in comments.
let b:match_words = &matchpairs
let b:match_skip = 's:comment\|string\|character'

setlocal commentstring=/*%s*/
setlocal expandtab
