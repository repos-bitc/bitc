;;; bitc.el --- BitC editing mode

;; Copyright (C) 1986, 87, 88, 97, 1998 Free Software Foundation, Inc.
;; Copyright (C) 2005, 2006, The EROS Group, LLC.

;; Author: Bill Rozas <jinx@martigny.ai.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;; Adapted-by: Jonathan S. Shapiro <shap.nospam@eros-os.org>
;; Keywords: languages, bitc
;;
;; This file is provides the BitC Editing Mode for GNU EMACS. It is
;; part of the BitC Compiler Distribution. While the bulk of the BitC
;; Compiler Distribution is distributed under the BSD Copyright, this
;; file is derived from the scheme.el implementation of GNU Emacs, and
;; is governed by the GNU General Public License.
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

;;; Commentary:

;; The major mode for editing BitC code, very similar to
;; the Lisp mode documented in the Emacs manual.
;;
;; Since BitC is not an interpreted langauge, there is no `run-bitc'
;; funcitonality.
;;
;; FIX: Need a recipe to generate BitC TAGS, something like the recipe
;; for DSSSL provided in scheme.el

;;; Code:

(require 'lisp-mode)

(defvar bitc-mode-syntax-table nil)
(if (not bitc-mode-syntax-table)
    (let ((i 0))
      (setq bitc-mode-syntax-table (make-syntax-table))
      (set-syntax-table bitc-mode-syntax-table)

      ;; Default is atom-constituent.
      (while (< i 256)
	(modify-syntax-entry i "_   ")
	(setq i (1+ i)))

      ;; Word components.
      (setq i ?0)
      (while (<= i ?9)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i ?A)
      (while (<= i ?Z)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))
      (setq i ?a)
      (while (<= i ?z)
	(modify-syntax-entry i "w   ")
	(setq i (1+ i)))

      ;; Whitespace
      (modify-syntax-entry ?\t "    ")
      (modify-syntax-entry ?\n ">   ")
      (modify-syntax-entry ?\f "    ")
      (modify-syntax-entry ?\r "    ")
      (modify-syntax-entry ?  "    ")

      ;; These characters are delimiters but otherwise undefined.
      ;; Brackets and braces balance for editing convenience.
      (modify-syntax-entry ?\[ "(]  ")
      (modify-syntax-entry ?\] ")[  ")
      (modify-syntax-entry ?{ "(}  ")
      (modify-syntax-entry ?} "){  ")
      (modify-syntax-entry ?\| "  23")

      ;; Other atom delimiters
      (modify-syntax-entry ?\( "()  ")
      (modify-syntax-entry ?\) ")(  ")
      (modify-syntax-entry ?\; "<   ")
      (modify-syntax-entry ?\" "\"    ")
;;      (modify-syntax-entry ?'  "  p")
      (modify-syntax-entry ?`  "  p")
;;      (modify-syntax-entry ?: "_ p")

      ;; Special characters
      (modify-syntax-entry ?, "_ p")
      (modify-syntax-entry ?@ "_ p")
      (modify-syntax-entry ?# "_ p14")
      (modify-syntax-entry ?\\ "\\   ")
      ))

(defvar bitc-mode-abbrev-table nil)
(define-abbrev-table 'bitc-mode-abbrev-table ())

;; shap: I have no idea how imenu works, so I'm not sure what to do
;; with this. In particular, I don't understand the role of -procedure
;; and friends
(defvar bitc-imenu-generic-expression
      '((nil
	 "^(define\\s-+(?\\(\\sw+\\)" 1)
	("Types"
	 "^(\\(defunion\\|defstruct\\|defexception\\)\\s-+(?\\(\\sw+\\)" 2)
	)
      "Imenu generic expression for BitC mode.  See `imenu-generic-expression'.")

(defun bitc-mode-variables ()
  (set-syntax-table bitc-mode-syntax-table)
  (setq local-abbrev-table bitc-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'fill-paragraph-function)
  (setq fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil)
  (make-local-variable 'normal-auto-fill-function)
  (setq normal-auto-fill-function 'lisp-mode-auto-fill)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lisp-indent-line)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'outline-regexp)
  (setq outline-regexp ";;; \\|(....")
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  ;; Look within the line for a ; following an even number of backslashes
  ;; after either a non-backslash or the line beginning.
  (setq comment-start-skip "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\);+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'lisp-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (make-local-variable 'lisp-indent-function)
  (set lisp-indent-function 'bitc-indent-function)
  (setq mode-line-process '("" bitc-mode-line-process))
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression bitc-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
	'(("+-*/.<>=?!$%_&~^:" . "w")))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '((bitc-font-lock-keywords
           bitc-font-lock-keywords-1 bitc-font-lock-keywords-2)
          nil nil (("+-*/.<>=!?$%_&~^" . "w")) beginning-of-defun
          (font-lock-mark-block-function . mark-defun))))

(defvar bitc-mode-line-process "")

(defvar bitc-mode-map nil
  "Keymap for BitC mode.
All commands in `lisp-mode-shared-map' are inherited by this map.")

(unless bitc-mode-map
  (let ((map (make-sparse-keymap "BitC")))
    (setq bitc-mode-map (make-sparse-keymap))
    (set-keymap-parent bitc-mode-map lisp-mode-shared-map)
    (define-key bitc-mode-map [menu-bar] (make-sparse-keymap))
    (define-key bitc-mode-map [menu-bar scheme]
      (cons "BitC" map))
    ;; (define-key map [run-scheme] '("Run Inferior Scheme" . run-scheme))
    (define-key map [uncomment-region]
      '("Uncomment Out Region" . (lambda (beg end)
                                   (interactive "r")
                                   (comment-region beg end '(4)))))
    (define-key map [comment-region] '("Comment Out Region" . comment-region))
    (define-key map [indent-region] '("Indent Region" . indent-region))
    (define-key map [indent-line] '("Indent Line" . lisp-indent-line))
    (put 'comment-region 'menu-enable 'mark-active)
    (put 'uncomment-region 'menu-enable 'mark-active)
    (put 'indent-region 'menu-enable 'mark-active)))


;;;###autoload
(defun bitc-mode ()
  "Major mode for editing BitC code.
Editing commands are similar to those of `lisp-mode'.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{bitc-mode-map}
Entry to this mode calls the value of `bitc-mode-hook'
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  (bitc-mode-initialize)
  (bitc-mode-variables)
  (run-hooks 'bitc-mode-hook))

(defun bitc-mode-initialize ()
  (use-local-map bitc-mode-map)
  (setq major-mode 'bitc-mode)
  (setq mode-name "BitC"))

(defgroup bitc nil
  "Editing BitC code"
  :group 'lisp)

(defcustom bitc-mode-hook nil
  "Normal hook run when entering `bitc-mode'.
See `run-hooks'."
  :type 'hook
  :group 'bitc)

(defconst bitc-font-lock-keywords-1 
  (eval-when-compile
    (list
     (list (concat "("
		   ;; pattern 1 matches initial keyword.
		   ;; pattern 2 matches define
		   ;; pattern 3 matches defunion/defstruct
		   "\\(\\(define\\)\\|\\(defunion\\|defstruct\\|defexception\\|deftype\\)\\)\\s-+"
		   ;; pattern 4 matches conditional curly brace
		   ;; pattern 5 matches name being defined
		   "\\((\\)?\\s-*\\(\\sw+\\)"
		   ;; pattern 6 matches conditional trailing lambda form
		   "\\(\\(?:\\s-\\|\n\\)+(\\(lambda\\)\\)?")
	   '(1 font-lock-keyword-face)
	   '(5 (cond ((match-beginning 3) font-lock-type-face)
		     ((match-beginning 6) font-lock-function-name-face)
		     ((match-beginning 4) font-lock-function-name-face)
		     (t font-lock-variable-name-face)))
	   )))
  "Subdued expressions to highlight in BitC modes.")

;;; (defconst bitc-font-lock-keywords-1 
;;;   '(("(\\(define\\)\\s-+\\(\sw+\\)"
;;;     (1 font-lock-keyword-face)
;;;     (2 font-lock-variable-name-face)))
;;;   )

;;; (defconst bitc-font-lock-keywords-1
;;;   (eval-when-compile
;;;     (list
;;;      ;;
;;;      ;; Declarations.
;;;      (list (concat "\\("
;;; 		   ;; Function names.
;;; 		   "\\((\\(define\\)\\s-+\\((\\sw+\\)\\s-+(\\(lambda\\)\\)"
;;; 		   "\\)\\>"
;;; 		   )
;;; 	   '(3 font-lock-keyword-face)
;;; 	   '(4 (cond ((match-beginning 2) font-lock-function-name-face)
;;; 		     (t font-lock-variable-name-face))
;;; 	       nil t))
;;;      ))

(defun in-type-qualifier (pt)
  (save-excursion
    (if (and (search-backward ":" (point-min) t)
	     (re-search-forward ":\\s-*" (point-max) t))
	(progn 
	  (forward-sexp)
	  (> (point) pt))
      nil)
    ))


(defconst bitc-font-lock-keywords-2
  (append bitc-font-lock-keywords-1
   (eval-when-compile
     (list
      ;;
      ;; Control structures.
      (cons
       (concat
	"(\\s-*" (regexp-opt
	     '(
	       "#f" "#t"
	       "and" "apply" "array" "array-length"
	       "array-nth" "assert"
	       "begin" "bitc-version" "bitfield"
	       "case" "case!" "catch" "check" "coindset"
	       "cond" "constrain" 
	       "deref" "declare" 
	       "definv" "defobject" "defthm"
	       "deftypeclass" "definstance" 
	       "exception" "external" 
	       "fill" "fn" "forall" "hide"
	       "if" "import" "import!" "indset"
	       "interface" 
	       "label" "lambda" "length"
	       "let" "letrec" "literal" "loop"
	       "make-vector" "member" "method"
	       "mutable"
	       "namespace" "nth"
	       "opaque" "or" "otherwise"
	       "pair" "proclaim" "provide" "provide!"
	       "read-only" "ref" "sensory"
	       "set!" "size-of" "super" "suspend"
	       "switch"
	       "the" "throw" "try"
	       "tycon" "tyfn" "typecase"
	       "use" "using" "val"
	       "vector" "vector-length" "vector-nth"
	       "word"
	       ;; Hannes Haug <hannes.haug@student.uni-tuebingen.de> wants:
	       ) t)
	"\\>") 1)

      ;;
      ;; Built-in types -- as separate words or following ":" with
      ;; possible white space
      (cons (concat "\\<"
		    (regexp-opt
		     '("int8" "int16" "int32" "int64"
		       "uint8" "uint16" "uint32" "uint64"
		       "char"  "bool"
		       "float" "double" "quad"
		       "bitfield") t)
		    "\\>") 'font-lock-type-face)
;;;       ;;
;;;       ;; Keywords that can be type, value, or pattern constructors, 
;;;       ;; according to context
;;;       (list (concat "\\<"
;;; 		    (regexp-opt
;;; 		     '("tuple") t)
;;; 		    "\\>") 
;;; 	    '(0 (cond ((in-type-qualifier (point)) font-lock-constant-face)
;;; 		      (t font-lock-type-face))))

      ;;
      ;; Non-keyword identifiers appearing in a type qualifier
      '("\\<\\(\\sw+\\)\\>"
	(1 (if (in-type-qualifier (match-beginning 1))
	       font-lock-type-face
	     nil)))
      ;;
      ;;BitC Type Variables
      '("'\\sw+\\>" . font-lock-type-face)
      )))
  "Gaudy expressions to highlight in BitC modes.")

(defvar bitc-font-lock-keywords bitc-font-lock-keywords-1
  "Default expressions to highlight in BitC modes.")


(defvar calculate-lisp-indent-last-sexp)

;; Copied from lisp-indent-function, but with gets of
;; bitc-indent-{function,hook}.
(defun bitc-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'bitc-indent-function)
			 (get (intern-soft function) 'bitc-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point normal-indent)))))))


;;; Let is different in Scheme

(defun would-be-symbol (string)
  (not (string-equal (substring string 0 1) "(")))

(defun next-sexp-as-string ()
  ;; Assumes that it is protected by a save-excursion
  (forward-sexp 1)
  (let ((the-end (point)))
    (backward-sexp 1)
    (buffer-substring (point) the-end)))

;; This is correct but too slow.
;; The one below works almost always.
;;(defun bitc-let-indent (state indent-point)
;;  (if (would-be-symbol (next-sexp-as-string))
;;      (bitc-indent-specform 2 state indent-point)
;;      (bitc-indent-specform 1 state indent-point)))

(defun bitc-let-indent (state indent-point normal-indent)
  (skip-chars-forward " \t")
  (if (looking-at "[-a-zA-Z0-9+*/?!@$%^&_:~]")
      (lisp-indent-specform 2 state indent-point normal-indent)
    (lisp-indent-specform 1 state indent-point normal-indent)))

;; (put 'begin 'bitc-indent-function 0), say, causes begin to be indented
;; like defun if the first form is placed on the next line, otherwise
;; it is indented like any other form (i.e. forms line up under first).

(put 'begin 'bitc-indent-function 0)
(put 'switch 'bitc-indent-function 'defun)
(put 'interface 'bitc-indent-function 1)
(put 'module 'bitc-indent-function 1)
(put 'proclaim 'bitc-indent-function 'defun)
(put 'typecase 'bitc-indent-function 1)
(put 'lambda 'bitc-indent-function 1)
(put 'let 'bitc-indent-function 'bitc-let-indent)
(put 'letrec 'bitc-indent-function 'bitc-let-indent)
(put 'loop 'bitc-indent-function 1)



(provide 'bitc)

;;; bitc.el ends here
