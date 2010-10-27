" Vim filetype plugin file
" Language:     BitC
" Maintainer:   Jonathan Shapiro <shap at eros-os org>
" Last Changed: 24 Sep 2010

if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

syn match bitcOperator "::"
syn match bitcOperator ":="
syn match bitcOperator "="
syn match bitcOperator "->"
syn keyword bitcOperator "not"
syn keyword bitcOperator "and"
syn keyword bitcOperator "or"

syn keyword bitcType int8 int16 int32 int64
syn keyword bitcType uint8 uint16 uint32 uint64
syn keyword bitcType char string bool
syn keyword bitcType float double quad

syn keyword bitcStructure struct union repr

syn keyword bitcConstant true false

syn keyword bitcKeyword import provide export module interface as
syn keyword bitcKeyword where def

syn keyword bitcStatement let letrec throw
" Should 'case' be bitcCase?
syn keyword bitcStatement try switch case otherwise
syn keyword bitcStatement in is do

syn keyword bitcConditional if then else when

syn keyword bitcRepeat loop until

syn match       bitcSpecial        display contained "\\\(x\x\+\|\o\{1,3}\|.\|$\)"
if !exists("bitc_no_utf")
  syn match     bitcSpecial        display contained "\\\(u\x\{4}\|U\x\{8}\)"
endif
syn region    bitcString         start=+L\="+ skip=+\\\\\|\\"+ end=+"+ contains=cSpecial,@Spell
syn match       bitcCharacter      "L\='[^\\]'"
syn match       bitcCharacter      "L'[^']*'" contains=bitcSpecial

"when wanted, highlight trailing white space
if exists("bitcc_space_errors")
  if !exists("bitcc_no_trail_space_error")
    syn match   bitcSpaceError     display excludenl "\s\+$"
  endif
  if !exists("bitc_no_tab_space_error")
    syn match   bitcSpaceError     display " \+\t"me=e-1
  endif
endif

" integer number, or floating point number without a dot and with "f".
syn case ignore
syn match       bitcNumbers        display transparent "\<\d\|\.\d" contains=bitcNumber,bitcFloat,bitcOctalError,bitcOctal
" Same, but without octal error (for comments)
syn match       bitcNumbersCom     display contained transparent "\<\d\|\.\d" contains=bitcNumber,bitcFloat,bitcOctal
syn match       bitcNumber         display contained "\d\+\(u\=l\{0,2}\|ll\=u\)\>"
"hex number
syn match       bitcNumber         display contained "0x\x\+\(u\=l\{0,2}\|ll\=u\)\>"
" Flag the first zero of an octal number as something special
syn match       bitcOctal          display contained "0\o\+\(u\=l\{0,2}\|ll\=u\)\>" contains=bitcOctalZero
syn match       bitcOctalZero      display contained "\<0"
syn match       bitcFloat          display contained "\d\+f"
"floating point number, with dot, optional exponent
syn match       bitcFloat          display contained "\d\+\.\d*\(e[-+]\=\d\+\)\=[fl]\="
"floating point number, starting with a dot, optional exponent
syn match       bitcFloat          display contained "\.\d\+\(e[-+]\=\d\+\)\=[fl]\=\>"
"floating point number, without dot, with exponent
syn match       bitcFloat          display contained "\d\+e[-+]\=\d\+[fl]\=\>"
if !exists("c_no_c99")
  "hexadecimal floating point number, optional leading digits, with dot, with exponent
  syn match     bitcFloat          display contained "0x\x*\.\x\+p[-+]\=\d\+[fl]\=\>"
  "hexadecimal floating point number, with leading digits, optional dot, with exponent
  syn match     bitcFloat          display contained "0x\x\+\.\=p[-+]\=\d\+[fl]\=\>"
endif

" flag an octal number with wrong digits
syn match       bitcOctalError     display contained "0\o*[89]\d*"

syn case match

syntax match  bitcCommentSkip contained "^\s*\*\($\|\s\+\)"
syntax region bitcComment matchgroup=bitcCommentStart start="/\*" end="\*/"

syntax region bitcCommentL start="//" skip="\\$" end="$" keepend

" Define the default highlighting.
" Only used when an item doesn't have highlighting yet.
highlight def link bitcCommentL bitcComment
highlight def link bitcCommentStart bitcComment
highlight def link bitcType Type
highlight def link bitcConstant Constant
highlight def link bitcStructure Structure
highlight def link bitcStatement Statement
highlight def link bitcRepeat Repeat
highlight def link bitcConditional Conditional
highlight def link bitcCharacter Character
highlight def link bitcSpecialCharacter SpecialCharacter
highlight def link bitcOctal Number
highlight def link bitcFloat Float
highlight def link bitcError Error
highlight def link bitcOctalError bitcError
highlight def link bitcSpaceError bitcError
highlight def link bitcSpecialError bitcError
highlight def link bitcConstant Constant
highlight def link bitcKeyword Keyword
highlight def link bitcOperator Keyword
highlight def link bitcComment Comment

let b:current_syntax = "bitc"
