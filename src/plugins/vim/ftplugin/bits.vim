" Vim filetype plugin file
" Language:     BitC
" Maintainer:   Jonathan Shapiro <shap at eros-os org>
" Last Changed: 24 Sep 2010

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Behaves just like BitC:
runtime! ftplugin/bitc.vim ftplugin/bitc_*.vim ftplugin/bitc/*.vim
