;;; bitc-imenu.el --- Font locking module for BitC Mode

;; Copyright 2010  Jonathan S. Shapiro
;; Copyright 2003, 2004, 2005, 2006, 2007, 2008  Free Software Foundation, Inc.
;; Copyright 1997-1998 Graeme E Moss, and Tommy Thorn
;;
;; Authors: Jonathan S. Shapiro <shap@eros-os.org>
;; Keywords: faces files BitC
;; Version: v1.0
;;
;; This file is not part of GNU Emacs.
;;
;; bitc-font-lock.el is derived in part from the haskell-font-lock
;; minor mode by Graeme E. Moss, Tommy Thorn, and Dave Love.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(eval-when-compile
  (require 'bitc-defs)
  (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMenu support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bitc-imenu-function-re
  (concat "^\\s-*"
          "\\(def\\)"
          "\\s-+\\(" bitc-qualified-ident-re "\\)\\>"

          ;; Skip white space:
          "\\s-*"

          ;; Two possibilities from this point:
          ;;
          ;; 1. This may be the convenience syntax, in which case we
          ;;    will immediately see a '('.
          ;; 2. This may be of the form def x {where ..} = lambda ..

          "\\(("                        ; the easy one
          "\\|"
          ;; The qualified ident may be followed by an optional
          ;; "where" clause.
          ;;
          ;; The where clause will in turn be followed by "=". We
          ;; don't need to consider the "is" case here, because that
          ;; is only legal in the function definition convenience
          ;; syntax, and we've already dealt with that.
          ;;
          ;; Skip to the "+" using the SHORTEST possible match to
          ;; ensure that we do not over-shoot:
          "\\([^=]*?=\s-*lambda\\)"

          "\\)"

          ;; Since we did a "shortest possible match" just above, the
          ;; "=" or "is" discovered will be the right one.
          )
  )

(defvar bitc-imenu-variable-re
  (concat "^\\s-*"
          "\\(def\\)"
          "\\s-+\\(" bitc-qualified-ident-re "\\)\\>"

          ;; Skip white space:
          "\\s-*"

          ;; Match optional "where" clause. Do not allow anything
          ;; before the "where", because that could only be a function
          ;; definition:

          "\\(where[^=]*\\)*?"

          "="

          ;; If we see lambda at this point it's a function, not a
          ;; variable, but it isn't worth the effort to filter those out.
          )
  )

(defvar bitc-imenu-declare-re
  (concat "^\\s-*"
          "\\(def\\)"
          "\\s-+\\(" bitc-qualified-ident-re "\\)\\>"

          ;; Skip white space:
          "\\s-*"

          ;; Match optional "where" clause. Do not allow anything
          ;; before the "where", because that could only be a function
          ;; definition:

          "\\(where\\(.|\n\\)*\\)*?"

          ":"

          ;; If we see lambda at this point it's a function, not a
          ;; variable, but it isn't worth the effort to filter those out.
          )
  )

(defvar bitc-imenu-type-definition-re
  (concat "^\\s-*"
          "\\(?:boxed\\|unboxed\\)"
          "\\s-+"
          "\\(struct\\|union\\|trait\\|repr\\)\\s-+"
          "\\(" bitc-qualified-ident-re
          ;; Match optional type arguments:
          "\\(\\s-*([^)]*)\\)?"
          "\\)"
          
          "\\s-*"
          ;; The [parameterized] type name will be followed by "is" or "{":
          "\\(\\(is\\)\\|{\\)"
          )
  )

(defvar bitc-imenu-exception-re
  (concat "^\\s-*"
          "\\(?:boxed\\|unboxed\\)?"
          "\\s-+"
          "\\(exception\\)\\s-+"
          "\\(" bitc-qualified-ident-re "\\)\\>"
          )
  )

(defvar bitc-imenu-generic-expression
  `((nil ,bitc-imenu-function-re 2)
    ("Types" ,bitc-imenu-type-definition-re 2)
    ("Functions" ,bitc-imenu-function-re 2)
    ("Variables" ,bitc-imenu-variable-re 2)
    ("Declarations" ,bitc-imenu-declare-re 2)
    ("Exceptions" ,bitc-imenu-exception-re 2)
    )

  "Imenu patterns for BitC-mode. See `imenu-generic-expression'.")

(provide 'bitc-imenu)
