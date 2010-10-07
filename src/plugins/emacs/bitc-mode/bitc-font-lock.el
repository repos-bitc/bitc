;;; bitc-font-lock.el --- Font locking module for BitC Mode

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
(require 'font-lock)

;; REMINDER:
;; 
;; The characters:
;;
;;   ! $ % & * + - / < > = ? ^ | ~
;;
;; are legal in operators. These were placed in the \sw (word)
;; character class for the current buffer in bitc-mode.el.
;;
;; We rely here on several regular expressions defined in bitc-mode-re.el
;;

(defcustom bitc-font-lock-symbols nil
  "Display -> and () and such using symbols in fonts.  This is
useful, but sometimes changes alignment and can therefore lead to
surprises w.r.t. layout. If t, tries to use whichever font is
available. Otherwise you can set it to a particular font of your
preference among `japanese-jisx0208' and `unicode'."
  :group 'bitc
  :type '(choice (const nil)
	         (const t)
	         (const unicode)
	         (const japanese-jisx0208)))

(defconst bitc-font-lock-symbols-alist
  (append
   (and (fboundp 'decode-char)
        (memq bitc-font-lock-symbols '(t unicode))
        (list (cons "\\" (decode-char 'ucs 955))))
   ;; Some symbols borrowed from JIS0208:
   (and (fboundp 'make-char) (fboundp 'charsetp) (charsetp 'japanese-jisx0208)        
        (memq bitc-font-lock-symbols '(t japanes-jisx0208))
	(list (cons "not" (make-char 'japanese-jisx0208 34 76))
	      (cons "->" (make-char 'japanese-jisx0208 34 42))))
   ;; Or a unicode font.
   (and (fboundp 'decode-char)
	(memq bitc-font-lock-symbols '(t unicode))
	(list (cons "not" (decode-char 'ucs 172))
              (cons "->" (decode-char 'ucs 8594))
;	      (cons "=>" (decode-char 'ucs 8658))
              (cons "()" (decode-char 'ucs #X2205))
              (cons "==" (decode-char 'ucs #X2261))
              (cons "!=" (decode-char 'ucs #X2262))
              (cons ">=" (decode-char 'ucs #X2265))
              (cons "<=" (decode-char 'ucs #X2264))
              (cons "&&" (decode-char 'ucs #X2227))
              (cons "||" (decode-char 'ucs #X2228))
              (cons "~>" (decode-char 'ucs 8669)) ;; Omega language
	      (cons "::" (decode-char 'ucs 8759))

;;   BitC does not currently use '.' for composition, but we may do so
;;   in the future. The Haskell version of dot-is-not-composition
;;   isn't really appropriate to BitC, so that hasn't been adapted.
;;
;;	      (list "." (decode-char 'ucs 8728) ; (decode-char 'ucs 9675)
;;                    ;; Need a predicate here to distinguish the . used by
;;                    ;; forall <foo> . <bar>.
;;                    'bitc-font-lock-dot-is-not-composition)
              )))
  "Alist mapping BitC symbols to chars.
Each element has the form (STRING . CHAR) or (STRING CHAR PREDICATE).
STRING is the BitC symbol.
CHAR is the character with which to represent this symbol.
PREDICATE if present is a function of one argument (the start position
of the symbol) which should return non-nil if this mapping should be disabled
at that position.")


;; Use new vars for the font-lock faces.  The indirection allows people to
;; use different faces than in other modes, as before.
(defvar bitc-keyword-face 'font-lock-keyword-face)
(defvar bitc-type-face 'font-lock-type-face)
(defvar bitc-builtin-face 'font-lock-builtin-face)
;; This used to be `font-lock-variable-name-face' but it doesn't result in
;; a highlighting that's consistent with other modes (it's mostly used
;; for function defintions).
(defvar bitc-definition-face 'font-lock-function-name-face)
(defvar bitc-operator-face 'font-lock-variable-name-face)
;; This is probably just wrong, but it used to use
;; `font-lock-function-name-face' with a result that was not consistent with
;; other major modes, so I just exchanged with `bitc-definition-face'.
(defvar bitc-variable-name-face 'font-lock-variable-name-face)
(defvar bitc-default-face nil)

(defconst bitc-emacs21-features (string-match "[[:alpha:]]" "x")
  "Non-nil if we have regexp char classes.
Assume this means we have other useful features from Emacs 21.")

(defun bitc-font-lock-compose-symbol (alist)
  "Compose a sequence of ascii chars into a symbol.
Regexp match data 0 points to the chars."
  ;; Check that the chars should really be composed into a symbol.
  (let* ((start (match-beginning 0))
         (end (match-end 0))
	 (syntaxes (cond
                    ((eq (char-syntax (char-after start)) ?w) '(?w))
                    ;; Special case for the . used for qualified names.
                    ((and (eq (char-after start) ?\.) (= end (1+ start)))
                     '(?_ ?\\ ?w))
                    (t '(?_ ?\\))))
         sym-data)
    (if (or (memq (char-syntax (or (char-before start) ?\ )) syntaxes)
	    (memq (char-syntax (or (char-after end) ?\ )) syntaxes)
	    (memq (get-text-property start 'face)
		  '(font-lock-doc-face font-lock-string-face
		    font-lock-comment-face))
            (and (consp (setq sym-data (cdr (assoc (match-string 0) alist))))
                 (let ((pred (cadr sym-data)))
                   (setq sym-data (car sym-data))
                   (funcall pred start))))
	;; No composition for you.  Let's actually remove any composition
	;; we may have added earlier and which is now incorrect.
	(remove-text-properties start end '(composition))
      ;; That's a symbol alright, so add the composition.
      (compose-region start end sym-data)))
  ;; Return nil because we're not adding any face property.
  nil)

(unless (fboundp 'char-displayable-p)
  (require 'latin1-disp nil t))

(defun bitc-font-lock-symbols-keywords ()
  (when (fboundp 'compose-region)
    (let ((alist nil))
      (dolist (x bitc-font-lock-symbols-alist)
	(when (and (if (fboundp 'char-displayable-p)
		       (char-displayable-p (if (consp (cdr x)) (cadr x) (cdr x)))
		     (if (fboundp 'latin1-char-displayable-p)
			 (latin1-char-displayable-p (if (consp (cdr x))
							(cadr x)
						      (cdr x)))
		       t))
		   (not (assoc (car x) alist)))	;Not yet in alist.
	  (push x alist)))
      (when alist
	`((,(regexp-opt (mapcar 'car alist) t)
	   (0 (bitc-font-lock-compose-symbol ',alist)
              ;; In Emacs-21, if the `override' field is nil, the face
              ;; expressions is only evaluated if the text has currently
              ;; no face.  So force evaluation by using `keep'.
              keep)))))))

(defconst bitc-font-lock-keywords
  (let (
        (type-names (concat "\\<"
                            (regexp-opt bitc-type-names)
                            "\\>"))
        (keywords (concat "\\<"
                          (regexp-opt bitc-other-keywords)
                          "\\>"))
        (builtins (concat "\\<"
                          (regexp-opt bitc-builtins)
                          "\\>"))
        (operators (concat  "\\<"
                            bitc-operator-re
                            "\\|\\(\\.\\.\\|::\\|∷\\)"
                            "\\>"))
        )
    
    `((,type-names . bitc-type-face)
      (,bitc-type-variable-re . bitc-type-face)
      (,bitc-version-re . bitc-keyword-face)
      (,keywords . bitc-keyword-face)
      (,builtins . bitc-builtin-face)
      (,operators . bitc-operator-face)
      (,(concat "^\\s-*def[ \t]+\\("
                bitc-qualified-ident-re
                "\\)")
       . bitc-definition-face)
;      (,(concat "\\<" bitc-qualified-ident-re "\\>")
;       . bitc-variable-name-face)
      ;("\\('\\w*'\\)" . bitc-variable-name-face)

      ;; String and character hilighting is taken care of using
      ;; bitc-mode-syntax-table
      ))
  "Highlighting expressions for BitC-mode")

(put 'bitc-mode 'font-lock-defaults '(bitc-font-lock-keywords nil t))

(defun turn-on-bitc-font-lock ()
  "Turns on font lock mode in current buffer for BitC scripts.

Changes the current buffer's `font-lock-defaults', and adds the
following variables:

  `bitc-keyword-face'       for reserved keywords and syntax
  `bitc-builtin-face'       for built-in language constructs
  `bitc-type-face'          for core type names
  `bitc-operator-face'      for mixfix operators
  `bitc-definition-face'    for the symbol being defined in a definition
  `bitc-variable-name-face' for ordinary code
  `bitc-default-face'       for ordinary code

The variables are initialized to the following font lock default
faces:

  `bitc-keyword-face'       `font-lock-keyword-face'
  `bitc-builtin-face'       `font-lock-builtin-face'
  `bitc-type-face'          `font-lock-type-face'
  `bitc-operator-face'      `font-lock-variable-name-face'
  `bitc-definition-face'    `font-lock-function-name-face'
  `bitc-variable-name-face' `font-lock-variable-name-face'
  `bitc-default-face'       <default face>

In addition, strings will be fontified using `font-lock-string-face'
according to the string syntactic category.

To alter an attribute of a face, add a hook.  For example, to change
the foreground colour of comments to brown, add the following line to
.emacs:

  (add-hook 'haskell-font-lock-hook
      (lambda ()
          (set-face-foreground 'haskell-comment-face \"brown\")))

The colours available vary from system to system.  To see what
colours are available on your system, call `list-colors-display'
from emacs.

To turn font locking on for all BitC buffers, add this to .emacs:

  (add-hook 'haskell-mode-hook 'turn-on-bitc-font-lock)

Invokes `bitc-font-lock-hook' if not nil."
  (set (make-local-variable 'font-lock-defaults) '(bitc-font-lock-keywords nil t))

  (run-hooks 'bitc-font-lock-hook)
  (turn-on-font-lock))

(defun turn-off-bitc-font-lock ()
  "Turns off font locking in current buffer."
  (font-lock-mode -1))

;; Provide ourselves:

(provide 'bitc-font-lock)
