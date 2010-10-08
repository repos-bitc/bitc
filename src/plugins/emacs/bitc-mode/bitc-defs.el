;;; bitc-defs.el --- Supporting definitions for BitC-mode

;; Copyright (C) 2010, Jonathan S. Shapiro
;; Author: Jonathan S. Shapiro <shap.nospam@eros-os.org>
;; Keywords: languages, bitc
;; Version: v1.0
;; URL: http://www.bitc-lang.org/
;;
;; This file is not part of GNU Emacs.
;;
;; This file is provides the BitC Editing Mode for GNU EMACS. It is
;; part of the BitC Compiler Distribution. While the bulk of the BitC
;; Compiler Distribution is distributed under the BSD Copyright, this
;; file is derived from previously existing GNU Emacs Lisp code, and
;; is therefore governed by the GNU General Public License.
;;
;; The BitC Editing Mode for GNU EMACS is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 2, or (at your option) any later version.
;;
;; The BitC Editing Mode for GNU EMACS is distributed in the hope that
;; it will be useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with BitC Compiler Distribution; see the file COPYING.  If
;; not, write to the Free Software Foundation, Inc., 59 Temple Place -
;; Suite 330, Boston, MA 02111-1307, USA.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Debugging support:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bitc-showme (format x)
  "A wrapper for `message' that returns its argument. This eliminates
the need for a result binding in most cases.

It's probably a good idea to add

  `(put 'bitc-showme 'lisp-indent-function 1)

to your .emacs file if you plan to edit this file."
  (message format x)
  x)

(defun bitc-dont-showme (format x)
  "Placeholder function to selectively turn off bitc-showme."
  x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper REs for bitc-mode.
;;
;; All of these REs are fully parenthesized for use in
;; composition. Most of these do not match anything for purposes of
;; positional matching, which allows them to be composed into larger
;; REs without perturbing what gets matched where. The general idea is
;; that you can build up using these, and wrap the ones you intend to
;; extract in \\( and \\) pairs.
;;
;; The exceptions are noted in their eldoc strings.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; BUG: this needs to be updated for multibyte, and I need to look at
;; the syntax-table stuff more closely before I can do that.
(defconst bitc-ident-characters "a-zA-Z0-9!\\$%&\\*\\+-/<>=\\?@#^\\|~\\."
  "Pattern for characters that are legal in BitC identifiers, for
use with \\[skip-chars-forward\\] and friends.")

(defconst bitc-ident-characters-re "\\(?:\\(?:[a-z][A-Z][0-9]!\\$%&\\*\\+-/<>=\\?@#^\\|~\\.\\)*\\)"
  "Pattern for matching remaining characters when searching for a
definition.")

;; The RE for bitc identifiers is depressingly complicated to the
;; human, so I build it up in parts:
(defconst bitc-operator-re "\\(?:[!$%&*+-/<>=?^|~]+\\)"
  "Regexp to match a BitC operator.")

(defconst bitc-ident-re
  (let* (;; Regexp to match an alpha chunk of an identifier:
         (bitc-alpha-chunk-re "\\(?:[a-zA-Z][a-zA-Z0-9]*\\)")

         ;; Regexp to match any identifier chunk:
         (bitc-ident-chunk-re   (concat "\\(?:"
                                        bitc-alpha-chunk-re
                                        "\\|"
                                        bitc-operator-re
                                        "\\)"))

         ;; Match a mixfix hole:
         (bitc-mixfix-hole-re "\\(?:@\\|_\\|\\(?:#_\\)\\)"))

    ;; Regexp to match a BitC identifier:
    (concat "\\(?:\\_<"
            "_*"                        ;leading underscores
            bitc-mixfix-hole-re "?"     ;optional mixfix hole
            bitc-ident-chunk-re
            "\\(?:" bitc-mixfix-hole-re bitc-ident-chunk-re "\\)*" ; more chunks sep by holes
            bitc-mixfix-hole-re "?"     ;optional mixfix hole
            "\\_>\\)"))
  "Regexp matching a BitC identifier. The regexp accounts for
leading underscores and holes.")

(defconst bitc-qualified-ident-re (concat "\\(?:"
                                bitc-ident-re
                                "\\(?:\\." bitc-ident-re "\\)*"
                                "\\)")
  "Regexp matching a (possibly qualified) BitC identifier.")

(defconst bitc-comment-re
  (let ((bitc-c-comment-re "\\(?:/\\*\\(?:[^*]\\|\\(?:\\*[^/]\\)\\)*\\*/\\)")
        (bitc-c++-comment-re "\\(?://\\(?:.*$\\)*\\)"))
    (concat "\\(?:" bitc-c-comment-re "\\|" bitc-c++-comment-re "\\)"))
  "Regexp matching a BitC comment..")

(defconst bitc-comment-and-whitespace-re
  (concat "\\(?:\\s-*" bitc-comment-re "\\s-*\\)")
  "Regexp matching a BitC comment, including leading and trailing white space.")

(defconst bitc-optional-semicolon-re
  "\\(?:\\s-;\\)?"
  "Regexp to match an optional semicolon.")

(defconst bitc-integer-re
  (concat "\\(?:"
          "\\(?:[1-9][0-9]*\\)"
          "\\|"
          "\\(?:0x[0-9a-fA-F]+\\)"
          "\\|"
          "\\(?:0o?[0-7]+\\)"
          "\\|"
          "\\(?:0b[01]+\\)"
          "\\)")
  "Regexp to match a bitc integer.")

(defconst bitc-character-re
  (concat
   "\\(?:'\\(?:"
   "[[:print:]]"                        ;regular printables (sort-of)
   "\\|"
   "\\\\[[:print:]]"          ;backslashed single characters (sort-of)
   "\\|"
   "\\\\[a-zA-Z0-9]+"           ;named characters, numbered characters
   "\\|"
   "\\(?:\\\\U\\+" bitc-integer-re "\\)"
   "\\)'\\)"
   )
  "Regexp to match a bitc character.")

(defconst bitc-string-re
  "\\(?:\"\\(?:[^\"]\\|\\\\\"\\)+\"\\)"
  "Regexp to match a bitc string.")

;; BUG: This one is sloppy - floating point values aren't allowed to
;; involve 0x and friends.
(defconst bitc-float-re
  (concat
   "\\(?:"
   bitc-integer-re
   "\\."
   bitc-integer-re
   "\\(?:[eE]" bitc-integer-re "\\)?"
   "\\)")
  "Regexp to match a bit floating-point value")

;; BUG: This one is sloppy - floating point values aren't allowed to
;; involve 0x and friends.
(defconst bitc-type-variable-re
  "\\(?:'[a-zA-Z_][a-zA-Z0-9_]*\\>\\)"
  "Regexp to match a type variable.")

(defconst bitc-version-number-re
  "\\(?:[0-9]+\\(?:\\.[0-9]+\\)*\\)"
  "Regexp to match a bitc version number in a bitc version statement.")

(defconst bitc-version-re 
  (concat "\\(?:"
          "bitc\\s-+version\\s-+"
          "\\(" bitc-version-number-re "\\)"
          bitc-optional-semicolon-re
          "\\)")
  "Regexp matching \"bitc version\" appearing at start of file
which is considered a keyword only in this position. The version
number matched is available as sub-expression 1.")

(defconst bitc-is-re "\\(\\<\\(is\\(\\s-*{\\)*?\\)\\|{\\)"
  "Regexp to search for next `is' construct, which is either an
open curly brace or `is' followed by an optional open curly
brace.")

(defvar bitc-top-form-names-re
  (concat "\\(?:\\<"
          (regexp-opt '("def" "struct" "union" "traint" "repr" "exception"
                        "boxed" "unboxed" "import" "provide"))
          "\\>\\)"))

(defvar bitc-top-level-form-re
  (concat "\\(?:\\s-*"
          "\\(?:\\(?:boxed\\|unboxed\\)\\s-+\\)?"
          "\\(?:\\(?:def\\|struct\\|union\\|trait\\|repr\\|exception\\|import\\|provide\\)\\s-+\\)"
          "\\(" bitc-qualified-ident-re "\\)\\>"
          "\\)")
  "Regexp to match the beginning of a top level form. The name of the
matched form is matched as sub-expression 1.")

(defvar bitc-uoc-form-re
  (concat "\\(?:\\s-*"
          "\\(?:module\\|interface\\|bitc\\)\\>"
          "\\)")
  "Regexp to match the beginning of a unit-of-compilation level form.")

(defvar bitc-beginning-of-form-re
  (concat "\\(?:"
          bitc-top-level-form-re
          "\\|"
          bitc-uoc-form-re
          "\\)")
  "Regexp to match beginning of current form.")

(defvar bitc-token-re
  (concat "\\(?:"
	  bitc-qualified-ident-re       ;also matches most keywords
                                        ;punctuation, and true/false
	  "\\|"
	  bitc-integer-re
	  "\\|"
	  bitc-float-re
	  "\\|"
	  bitc-version-number-re
	  "\\|"
	  bitc-character-re
	  "\\|"
	  bitc-string-re
	  "\\|"
	  bitc-type-variable-re
	  "\\|"
	  "[][{}():;,=]"                ;single-character tokens
	  "\\|"
	  ":="                          ;assignment
	  "\\|"
	  "::"                          ;list construction
	  "\\|"
	  "->"                          ;type arrow
	  "\\)")
  "Regexp to match a single BitC token")

(defconst bitc-continuation-tokens-re
  (concat "\\(?:\\<"
          (regexp-opt '("else" "then" "case" "catch" "otherwise"
                        "until" "in" "is" "where" "do"))
          "\\>\\)")
  "Regexp that matches only continuation tokens.")

;; I'm not convinced that builtins are distinct from keywords for any
;; purpose other than font-lock:
(defvar bitc-builtins
  '(
    "and"
    "case"
    "do"
    "else"
    "if"
    "in"
    "is"
    "let"
    "letrec"
    "loop"
    "or"
    "otherwise"
    "switch"
    "then"
    "try"
    "until"
    "when"
    "while"
    )
  "List of strings that should be treated as built-ins.")

(defvar bitc-other-keywords
  '(
    "as"
    "boxed"
    "const" 
    "continue"
    "def"
    "exception"
    "from"
    "import"
    "instance"
    "module"
    "provide"
    "repr"
    "return"
    "struct"
    "trait"
    "unboxed"
    "union"
    )
  "List of keywords not included in bitc-builtins")

(defvar bitc-keywords
  (append bitc-builtins bitc-other-keywords)
  "List of strings that are BitC keywords.")
  
(defvar bitc-type-names
  '(
    "int8"
    "int16"
    "int32"
    "int64"
    "uint8"
    "uint16"
    "uint32"
    "uint64"
    "word"
    "char"
    "string"
    "bool"
    "float"
    "double"

    "const"
    "mutable"
    )
  "List of BitC core type names.")

(defconst bitc-defun-re       "\\(?:\\<\\(?:def\\|module\\|interface\\)\\>\\)"
  "Regexp matching the beginning of a function or module")
(defconst bitc-modbegin-re    "\\(?:\\<\\(?:module\\|interface\\)\\>\\)"
  "Regexp matching the beginning of a module")

;; Following list should match the tok.tokType values of wantAutoSemi
;; in TransitionLexer.cxx, excluding ';' and '}'.
(defconst bitc-continuation-line-re
  (concat "\\(?:^\\s-*\\(?:"
          "[])]"                    ;leading single character tokens
          "\||"
          (concat "\\(?:\\<"
                  (regexp-opt '("then" "else" "case" "catch" "otherwise" "until"
                                "in" "is" "where" "do") t)
                  "\\>\\)")
          "\\)\\)")
  "Regular expression matching lines whose leading token can only
appear on a continuation line.")

;; Following should match lines whose tail matches one of the
;; lastTokType values of wantAutoSemi in TransitionLexer.cxx,
;; excluding ';' and '{'
(defconst bitc-continued-line-re 
  (concat "\\(?:^.*"                    ;beginning of line
          "[],(]"                    ;ends in ',' '(' or '['
          "\\s-*$\\)")               ;end of line
  "Regexp matching lines whose successor line is a continuation line.")

;; Following should closely match the value of curlyRequired in TransitionLexer.cxx
(defconst bitc-line-starts-block-re
  (concat "\\(?:^.*"                    ;beginning of line
          "\\(?:"                       ;start of group
          "{"                           ;ends in '{'
          "\\|"                         ;or
          (concat "\\(?:\\<"            ;ends in block keyword
                  (regexp-opt '("let" "letrec" "loop" "switch" "in" "is" "do" "try"
                                "then" "otherwise" "throw" "else") t)
                  "\\>\\)")
          "\\)"                         ;end of group
          "\\s-*$\\)")                  ;end of line
  "Regexp matching lines that are followed by a new block")


(defconst bitc-exclude-str-start "/* -----\\/----- EXCLUDED -----\\/-----")
(defconst bitc-exclude-str-end "   -----/\\----- EXCLUDED -----/\\-----*/")

(provide 'bitc-defs)
