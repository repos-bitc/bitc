;;; bitc-mode.el --- BitC editing mode

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

(defgroup bitc nil
  "Major mode for editing BitC source in Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

;;;###autoload
(add-to-list 'load-path
   (or (file-name-directory load-file-name) (car load-path)))
(add-to-list 'auto-mode-alist '("\\.bitc\\'" . bitc-mode))
(add-to-list 'auto-mode-alist '("\\.bits\\'" . bitc-mode))

(eval-when-compile (require 'cl))
(eval-when-compile
  ;; Emacs 21 defines `values' as a (run-time) alias for list.
  ;; Don't maerge this with the pervious clause.
  (if (string-match "values"
		    (pp (byte-compile (lambda () (values t)))))
      (defsubst values (&rest values)
	values)))

(require 'bitc-defs)
(require 'bitc-parse)
(require 'bitc-imenu)
(require 'bitc-font-lock)

(defvar bitc-mode-abbrev-table nil
  "Abbrev table in use in BitC-mode buffers.")
(define-abbrev-table 'bitc-mode-abbrev-table ())

(defvar bitc-mode-hook nil)

(defvar bitc-mode-map
  (let ((map (make-keymap)))
    ;; Issue: no support for electric anything mode. Should we?

    (define-key map "\C-j" 'newline-and-indent)
    (define-key map "\M-\C-h" 'bitc-mark-form)
    (define-key map "\M-\C-a" 'bitc-beginning-of-form)
    (define-key map "\M-\C-e" 'bitc-end-of-form)
    (define-key map "\C-c\C-d" 'bitc-goto-definition)
    (define-key map "\C-c\C-c" 'bitc-comment-area)
    (define-key map "\C-c\C-i" 'bitc-indentation-levels) ;for testing
    (define-key map "\C-c\C-t" 'bitc-next-token) ;for testing
    (define-key map "\C-c\C-u" 'bitc-uncomment-area)
    (define-key map "\n" 'newline-and-indent)
    (define-key map "\r" 'newline-and-indent)
    (define-key map "\d" 'bitc-delete-backward-char)
    (define-key map "\C-d" 'bitc-delete-char)
;;    (define-key map "\C-c\C-o" 'bitc-outline-mode)
    map)
  "Keymap for BitC major mode")

(defvar bitc-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Underscores and holes in identifiers:
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?@ "w" st)
    (modify-syntax-entry ?# "w" st)

    ;; Operator characters need to have identifier symbol syntax:
    (modify-syntax-entry ?! "_" st)
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?& "_" st)
    (modify-syntax-entry ?* "_" st)
    (modify-syntax-entry ?+ "_" st)
    (modify-syntax-entry ?- "_" st)
    (modify-syntax-entry ?/ "_" st)
    (modify-syntax-entry ?< "_" st)
    (modify-syntax-entry ?> "_" st)
    (modify-syntax-entry ?= "_" st)
    (modify-syntax-entry ?? "_" st)
    (modify-syntax-entry ?^ "_" st)
    (modify-syntax-entry ?| "_" st)
    (modify-syntax-entry ?~ "_" st)

    ;; Comment syntax:
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)

    ;; Strings, Characters, Escape sequences:
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "/" st)
    (modify-syntax-entry ?\\ "\\" st)

    ;; Punctuation:
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?: "." st)
    (modify-syntax-entry ?. "." st)

    ;; Parenthesis:
    (modify-syntax-entry ?( "()" st)
    (modify-syntax-entry ?) ")(" st)
    (modify-syntax-entry ?[ "(]" st)
    (modify-syntax-entry ?] ")[" st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)

    ;; Expression prefix:
    (modify-syntax-entry ?' "'" st)

    st)
  "Syntax table for BitC-mode")

(defun bitc-mode ()
  "Major mode for editing BitC source files. \\<bitc-mode-map>
TAB indents for BitC code.

\\[bitc-complete-word] completes the word around current point with respect
to position in code
\\[bitc-show-completions] shows all possible completions at this point.

Other useful functions are:

\\[bitc-comment-area]\t- Put marked area in a comment, fixing nested comments.
\\[bitc-uncomment-area]\t- Uncomment an area commented with \
\\[bitc-comment-area].
\\[bitc-goto-definition]\t- Goto definition prompted for in minibuffer.

When the mode provides some form of indent support, this description will 
say something about the billions of variables involved.

Turning on BitC mode calls the value of hte variable bitc-mode-hook with
no args, if that value is non-nill."

  (interactive)
  (kill-all-local-variables)
  (use-local-map bitc-mode-map)
  (setq major-mode 'bitc-mode)
  (setq mode-name "BitC")
  (setq local-abbrev-table bitc-mode-abbrev-table)
  (set-syntax-table bitc-mode-syntax-table)

  (set (make-local-variable 'indent-line-function) 'bitc-indent-line)
;  (set (make-local-variable 'comment-indent-function) '(bitc-indent-comment))
  (set (make-local-variable 'case-fold-search) t)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-start-skip) "\\(//+\\|/\\*+\\)\\s *")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) " ")
  ;;  (set (make-local-variable 'comment-continue) " *")
;;  (set (make-local-variable 'comment-indent-function) bitc-comment-indent-function)

  (set (make-local-variable 'font-lock-defaults)
       '(bitc-font-lock-keywords nil t))

  (set (make-local-variable 'imenu-generic-expression) bitc-imenu-generic-expression)
  (setq imenu-case-fold-search t)

  (imenu-add-menubar-index)

  (turn-on-bitc-font-lock)

;;  (run-hooks bitc-mode-hook)
)

(defun bitc-mark-form ()
  "Mark the BitC function (or procedure).
This puts the mark at the end, and point at the beginning."
  (interactive)
  (bitc-end-of-form)
  (push-mark (point) nil t)
  (bitc-beginning-of-form)
)

(defun bitc-comment-area (start end)
  "Put the region into a BitC comment.
The comments that are in this area are \"deformed\":
`/*' becomes `/!*' and `*/' becomes `*!/'.
These deformed comments are returned to normal if you use
\\[bitc-uncomment-area] to undo the commenting.

The commented area starts with `bitc-exclude-str-start', and ends with
`bitc-include-str-end'.  But if you change these variables,
\\[bitc-uncomment-area] won't recognize the comments."
  (interactive "r")
  (save-excursion
    ;; Insert start and endcomments
    (goto-char end)
    (if (and (save-excursion (skip-chars-forward " \t") (eolp))
	     (not (save-excursion (skip-chars-backward " \t") (bolp))))
	(forward-line 1)
      (beginning-of-line))
    (insert bitc-exclude-str-end)
    (setq end (point))
    (newline)
    (goto-char start)
    (beginning-of-line)
    (insert bitc -exclude-str-start)
    (newline)
    ;; Replace start and end comments within commented area
    (goto-char end)
    (save-excursion
      (while (re-search-backward "\\*/" start t)
	(replace-match "*!/" t t)))
    (save-excursion
      (while (re-search-backward "/\\*" start t)
	(replace-match "/!*" t t)))))

(defun bitc-uncomment-area ()
  "Uncomment a commented area; change deformed comments back to normal.
This command does nothing if the pointer is not in a commented
area.  See also `bitc-comment-area'."
  (interactive)
  (save-excursion
    (let ((start (point))
	  (end (point)))
      ;; Find the boundaries of the comment
      (save-excursion
	(setq start (progn (search-backward bitc-exclude-str-start nil t)
			   (point)))
	(setq end (progn (search-forward bitc-exclude-str-end nil t)
			 (point))))
      ;; Check if we're really inside a comment
      (if (or (equal start (point)) (<= end (point)))
	  (message "Not standing within commented area.")
	(progn
	  ;; Remove endcomment
	  (goto-char end)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point))))
	  ;; Change comments back to normal
	  (save-excursion
	    (while (re-search-backward "/!\\*" start t)
	      (replace-match "/*" t t)))
	  (save-excursion
	    (while (re-search-backward "\\*!/" start t)
	      (replace-match "*/" t t)))
	  ;; Remove startcomment
	  (goto-char start)
	  (beginning-of-line)
	  (let ((pos (point)))
	    (end-of-line)
	    (delete-region pos (1+ (point)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bitc-block-openers-re
  "^.*\\<\\({\\|in\\|then\\|else\\|do\\|try\\|is\\until\\)\\s-*$"
  "Regexp for looking to see if we are the first line in a newly
opened block.")
(defvar bitc-binding-sequence-prefix-re
  "^.*\\<\\(let\\|letrec\\|loop\\)\\>\\s-*"
  "Regexp that matches an open binding sequence up to the first
position of the first bound identifier.")

;; FIX: This probably needs to confirm that we have an ident on the
;; same line as the binding keyword
(defun bitc-current-line-is-open-binding-sequence ()
  (interactive)
  (bitc-showme "Is open binder: %S"
    (save-excursion
      (beginning-of-line)
      (or (and (looking-at "^.*\\<\\(let\\|letrec\\)\\>")
               (not (looking-at "^.*\\<in\\>\\(\\s-*{\\s-*\\)?\\s-*$")))
          (and (looking-at "^.*\\<\\(loop\\)\\>")
               (not (looking-at "^.*\\<until\\>\\(\\s-*{\\s-*\\)?\\s-*$")))))))
    
(defun bitc-current-line-begins-block ()
  "Return t if the current line is the first line in a block, nil otherwise."
  (interactive)
  (save-excursion
    (bitc-showme "Begins block: %S" 
      (progn
        (beginning-of-line)                
        (forward-comment (- (point)))
        (looking-back bitc-block-openers-re)))))

;; Bug: Currently assumes that we aren't in a comment.
(defun bitc-current-block-indent ()
  "Computes the indent level for elements of the current
block. Returns a triple (block-indent continue-indent containing-indent)."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (bitc-showme "Block body rules are %S" 
      (let* ((is-block-opener (looking-at bitc-block-openers-re))
             (starting-line-indent (bitc-current-line-current-indent)))
        (cond
         ((looking-at "^\\s-*bitc\\s-+version") (list 0 0 0))

         ((looking-at "^\\s-*\\(module\\|interface\\)") (list 0 0 0))

         ;; Top-level forms get indented one block level
         ((looking-at bitc-top-level-form-re)  
          (list (bitc-current-line-current-indent)
                (bitc-current-line-current-indent)
                0))

         (t (forward-comment (- (point)))
            (while (and (>= (bitc-current-line-current-indent) starting-line-indent)
                        (not (or (bobp)
                                 (looking-back bitc-block-openers-re)
                                 (looking-back "}\\(\\s-*;\\)*$")
                                 (bitc-current-line-is-open-binding-sequence))))
              (beginning-of-line)
              (forward-comment (- (point))))

            (beginning-of-line)
            (cond
             ((looking-at "^.*}\\(\\s-*;\\)*$")
              (bitc-current-block-indent))
             ((and (bitc-current-line-is-open-binding-sequence)
                   (not is-block-opener))
              (let* ((containing-indent (bitc-current-line-current-indent))
                     (indent-level (progn
                                     (re-search-forward bitc-binding-sequence-prefix-re)
                                     (current-column))))
                (re-search-forward "[^=]*=\\s-*" nil 'move)
                (list indent-level 
                      (+ (current-column)
                         bitc-continued-line-indent)
                      containing-indent)))

             (t (let* ((containing-indent (bitc-current-line-current-indent))
                       (indent-level (+ containing-indent bitc-block-indent))
                       (continue-level (+ indent-level bitc-continued-line-indent)))
                  (list indent-level continue-level containing-indent))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The real work-horse of the indent system
;;
;; The correct indent to use is often (but not always) ambiguous
;; because of the way layout works on the input. In a block of code,
;; there are generally two choices:
;;
;;  while true do
;;    1 +
;;      2
;;    ^ ^
;;    | Continuation line level
;;    Block indent level
;;
;; In a binding context, there are three options:
;;
;;   let x = 1 +
;;           2
;;       ^   ^
;;       |   Continuation line level
;;       Basic indent level
;;
;; While we need to offer both choices in response to the tab key,
;; when matters are ambiguous the preferred choice in each case is the
;; leftmost choice.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun bitc-compute-line-indent ()
  (interactive)
  (save-excursion
    (bitc-showme "Indent info is %S"
      (beginning-of-line)
      (let* ((block-indents (bitc-current-block-indent))
             (bol (point)))
        (if (and (consp bitc-line-indent-info)
                 (= (point) (bitc-cycle-pos bitc-line-indent-info)))
            bitc-line-indent-info
          (setq bitc-line-indent-info
                (cond
                 ((bitc-current-line-begins-block)
                  (bitc-make-cycle bol (car block-indents)))

                 ((looking-at "^\\s-*}\\(\\s-*;\\)?")
                  (bitc-make-cycle bol (car block-indents)))

                 ;; Things that *must* be a continuation line:
                 ((looking-back "\\([,(=[]\\|:=\\)\n")  
                  (bitc-make-cycle bol (cadr block-indents)))

                 ;; If the current line is indented beyond the current
                 ;; block indent, then it is either a continuation line or
                 ;; an additional let binding. Back up to the preceding
                 ;; line that is indented at block level and see what it is:
                 ;;
                 ;; Note that if the let[rec] has been closed, we caught
                 ;; the trailing "in" above, so we don't need to check that
                 ;; here.
                 ((> (bitc-current-line-current-indent) (car block-indents))
                  (while (> (bitc-current-line-current-indent)
                            (car block-indents))
                    (forward-line -1))
                  (cond
                   ((bitc-current-line-is-open-binding-sequence)
                    (bitc-make-cycle bol (car block-indents) (cadr block-indents)))

                   ((looking-at "^\\s-*}")
                    (bitc-make-cycle bol (car block-indents)))

                   ;; Otherwise prefer a continuation line
                   (t (bitc-make-cycle bol
                                       (cadr block-indents) (car block-indents)))))

                 ;; Otherwise, report the current block indent, with option
                 ;; to indent further:
                 (t (bitc-make-cycle bol (car block-indents) (cadr block-indents)))
                 )))
        bitc-line-indent-info))))

;(defun bitc-indent-line ()
;  (interactive)
;  (let* ((info (bitc-compute-line-indent))
;        (cur-indent (bitc-current-line-current-indent)))
;    (bitc-cycle-indent cur-indent info)))
           
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Completion -- mostly not implemented
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bitc-str nil)
(defvar bitc-all nil)
(defvar bitc-pred nil)
(defvar bitc-buffer-to-use nil)
(defvar bitc-flag nil)

(defun bitc-string-diff (str1 str2)
  "Return index of first letter where STR1 and STR2 differs."
  (catch 'done
    (let ((diff 0))
      (while t
	(if (or (> (1+ diff) (length str1))
		(> (1+ diff) (length str2)))
	    (throw 'done diff))
	(or (equal (aref str1 diff) (aref str2 diff))
	    (throw 'done diff))
	(setq diff (1+ diff))))))

(defun bitc-get-default-symbol ()
  "Return symbol around current point as a string."
  (save-excursion
    (buffer-substring (progn
			(skip-chars-backward " \t")
			(skip-chars-backward bitc-ident-characters)
			(point))
		      (progn
			(skip-chars-forward bitc-ident-characters)
			(point)))))

(defun bitc-completion-response ()
  (cond ((or (equal bitc-flag 'lambda) (null bitc-flag))
	 ;; This was not called by all-completions
	 (if (null bitc-all)
	     ;; Return nil if there was no matching label
	     nil
	   ;; Get longest string common in the labels
	   (let* ((elm (cdr bitc-all))
		  (match (car bitc-all))
		  (min (length match))
		  tmp)
	     (if (string= match bitc-str)
		 ;; Return t if first match was an exact match
		 (setq match t)
	       (while (not (null elm))
		 ;; Find longest common string
		 (if (< (setq tmp (bitc-string-diff match (car elm))) min)
		     (progn
		       (setq min tmp)
		       (setq match (substring match 0 min))))
		 ;; Terminate with match=t if this is an exact match
		 (if (string= (car elm) bitc-str)
		     (progn
		       (setq match t)
		       (setq elm nil))
		   (setq elm (cdr elm)))))
	     ;; If this is a test just for exact match, return nil ot t
	     (if (and (equal bitc-flag 'lambda) (not (equal match 't)))
		 nil
	       match))))
	;; If flag is t, this was called by all-completions. Return
	;; list of all possible completions
	(bitc-flag
	 bitc-all)))

(defun bitc-build-def-re (str &optional arg)
  "Return function/procedure starting with regular expression STR.
With optional second arg non-nil, STR is the complete name of the instruction."
  (let ((target (if arg str (concat str bitc-ident-characters-re))))
    (concat "^\\s-*"
            "\\(\\(?:boxed\\|unboxed\\)?\\s-*"
            "\\(?:def\\|struct\\|union\\|trait\\|repr\\)\\)\\s-+"
            "\\(" target "\\)\\>")))

;; Function passed to completing-read, try-completion or
;; all-completions to get completion on any function name. If
;; predicate is non-nil, it must be a function to be called for every
;; match to check if this should really be a match. If flag is t, the
;; function returns a list of all possible completions. If it is nil
;; it returns a string, the longest possible completion, or t if STR
;; is an exact match. If flag is 'lambda, the function returns t if
;; STR is an exact match, nil otherwise.

(defun bitc-complete-def (bitc-str bitc-pred bitc-flag)
  (save-excursion
    (let ((bitc-all nil)
	  match)

      ;; Set buffer to use for searching labels. This should be set
      ;; within functions which use bitc-completions
      (set-buffer bitc-buffer-to-use)

      (let ((bitc-str bitc-str))
	;; Build regular expression for functions
	(if (string= bitc-str "")
	    (setq bitc-str (bitc-build-def-re bitc-ident-re t))
	  (setq bitc-str (bitc-build-def-re bitc-str)))
	(goto-char (point-min))

	;; Build a list of all possible completions
	(while (re-search-forward bitc-str nil t)
	  (setq match (buffer-substring (match-beginning 2) (match-end 2)))
	  (if (or (null bitc-pred)
		  (funcall bitc-pred match))
	      (setq bitc-all (cons match bitc-all)))))

      ;; Now we have built a list of all matches. Give response to caller
      (bitc-completion-response))))

(defun bitc-goto-definition ()
  "Move to specified BitC definition.
The default is a name found in the buffer around point."
  (interactive)
  (let* ((default (bitc-get-default-symbol))
	 ;; The following variable is used in bitc-comp-function
	 (bitc-buffer-to-use (current-buffer))
	 (default (if (bitc-complete-def default nil 'lambda)
		      default ""))
	 (label (if (not (string= default ""))
		    ;; Do completion with default
		    (completing-read (concat "Label (default " default "): ")
				     'bitc-complete-def nil t "")
		  ;; There is no default value. Complete without it
		  (completing-read "Label: "
				   'bitc-complete-def nil t ""))))
    ;; If there was no response on prompt, use default value
    (if (string= label "")
	(setq label default))
    ;; Goto right place in buffer if label is not an empty string
    (or (string= label "")
	(progn
	  (goto-char (point-min))
	  (re-search-forward (bitc-build-def-re label t))
	  (beginning-of-line)))))

(provide 'bitc)

;;; bitc.el ends here
