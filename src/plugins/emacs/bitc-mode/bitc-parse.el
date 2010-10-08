;;; bitc-parse.el --- BitC editing mode

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

(eval-when-compile
  (require 'bitc-defs)
  (require 'cl))

(defun bitc-beginning-of-current-line ()
  (save-excursion
    (beginning-of-line)
    (point)))

(defun bitc-in-left-indent-areap (pos inclusive)
  "Return t if `pos' falls within the left indent area. If `inclusive'
is non-nil, also returns t when point is on the first non-white character of the line."
  (interactive "d")
  (bitc-showme "In left indent area: %S" 
    (save-excursion
      (beginning-of-line)
      (and (>= pos (point))
           (progn
             (skip-chars-forward " ")
             (or (< pos (point))
                 (and inclusive
                      (= pos (point)))))))))

(defun bitc-current-line-current-indent ()
  (interactive)
  (save-excursion
    (beginning-of-line)

    ;; If the current line is completely blank, then the effective
    ;; indent is that of the nearest preceding non-blank line. But if
    ;; the current line has leading spaces, those were probably
    ;; inserted by bitc-indent-line, and we should take them as the
    ;; proper current state.
    (when (looking-at "^$")
      (forward-comment (- (point)))
      (beginning-of-line))

    (let ((col (progn (skip-chars-forward " ")
                      (current-column))))
      (bitc-showme "Current line indent is %S" col))))

(defun bitc-current-line-blankp ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun bitc-continuation-linep ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (looking-at bitc-continuation-tokens-re)))

(defun bitc-beginning-of-form ()
  "Move backward to the beginning of the current definition/declaration."
  (interactive)
  (beginning-of-line)
  (if (not (looking-at bitc-beginning-of-form-re))
      (re-search-backward bitc-beginning-of-form-re nil 'move)))

(defun bitc-end-of-block ()
  "Move forward to the end of the block that starts on the current line."
  (let ((start-indent (bitc-current-line-current-indent))
        (starting-line-pos (line-beginning-position)))
    (re-search-forward "\\(.\\|\n\\)*?\\(=\\|is\\|{\\)" nil 'move)
    (if (looking-at "{")
        (forward-sexp 1)
      (progn 
        (goto-char starting-line-pos)
        (forward-line 1)
        (while (and (not (eobp))
                    (> (bitc-current-line-current-indent) start-indent))
          (forward-sexp))

        (beginning-of-line)
        ;; Back up across comments and white space:
        (forward-comment (- (point)))
        (skip-chars-forward " \t")
        (if (looking-at "\n")
            (forward-char))))))

(defun bitc-end-of-form ()
  "Move forward to the end of the current definition/declaration."
  (interactive)
  (bitc-beginning-of-form)
  (bitc-end-of-block))

(defun bitc-implicit-modulep ()
  (save-excursion
    (beginning-of-buffer)
    (forward-comment (buffer-size))
    (when (looking-at bitc-version-number-re)
      (goto-char (match-end))
      (forward-comment (buffer-size)))
    (not (looking-at bitc-uoc-form-re))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Tokenization. A token takes the form ('token "string" col start end)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bitc-make-token (s col start end)
  (list 'token s col start end))

(defun bitc-get-token-str (tok)
  (nth 1 tok))

(defun bitc-get-token-col (tok)
  (nth 2 tok))

(defun bitc-get-token-start (tok)
  (nth 3 tok))

(defun bitc-get-token-end (tok)
  (nth 4 tok))

(defun bitc-next-token ()
  "Returns the next token in the form (string column-number start-pos end-pos),
moving forward over the token if one is found."
  (interactive)
  (forward-comment (buffer-size))
  (bitc-showme "Next token %S"
    (let ((col (current-column)))
      (cond
       ((looking-at bitc-token-re)
        (goto-char (match-end 0))
        (bitc-make-token (match-string 0) col
                         (match-beginning 0) (match-end 0)))
       (t (bitc-make-token "<EOF>" -1 (buffer-size) (buffer-size)))))))

(defun bitc-peek-next-token ()
  "Returns the next token in the form (string position column-number),
without moving forward."
  (interactive)
  (save-excursion
    (bitc-next-token)))

(defun bitc-next-token-indent ()
  "Returns the column-number of the next BitC token."
  (interactive)                         ;for debugging
  (save-excursion
    (forward-comment (buffer-size))
    (bitc-showme "Next token indent is %S" (current-column))))
  
(defun bitc-forward-token ()
  ;; Just discard the result:
  (bitc-next-token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bitc-top-indent 2
  "Amount to indent top-level forms relative to the containing module.")
(defvar bitc-block-indent 2
  "Amount to indent content of a block relative to its containing content.")
(defvar bitc-continued-line-indent 2
  "Amount to indent a continued line relative to its start line.")
(defvar bitc-in-block-indent 3
  "Amount to indent a continued line relative to its start line.")
(defvar bitc-let-binding-indent 4
  "Amount to indent an additional let binding.")
(defvar bitc-letrec-binding-indent 7
  "Amount to indent an additional letrec binding.")

(defun bitc-relative-indent (str)
  (cond ((equal str "let") bitc-let-binding-indent)
        ((equal str "letrec") bitc-letrec-binding-indent)
        ((equal str "in") bitc-in-block-indent)
        ((equal str ",") bitc-continued-line-indent)
        ((equal str "(") bitc-continued-line-indent)
        ((equal str "[") bitc-continued-line-indent)
        ((equal str "=") bitc-continued-line-indent)
        ((equal str ":=") bitc-continued-line-indent)
        (t bitc-block-indent)))

;; Fix: The top of the file isn't edited very often. See if there is a
;; way we can memoize this via some form of "trigger on region"to
;; avoid going hither and yon on every indent.
(defun bitc-top-form-indent-level ()
  (interactive)                         ;for debugging
  (bitc-showme "Top level forms indent at %S"
    (save-excursion
      (beginning-of-buffer)
      (forward-comment (buffer-size))
      (when (looking-at bitc-version-number-re)
        (goto-char (match-end))
        (forward-comment (buffer-size)))
      (forward-comment (buffer-size))
      (if (looking-at "\\(module\\|interface\\)")
          (progn
            (re-search-forward bitc-is-re nil 'move)
            (bitc-next-token-indent))
        0))))
                    
;; Attempt #2: Return a list of candidate indentation levels for the
;; current line. This is a list of all of the indentations to the left
;; of the present line position, plus the current indent level. In
;; practice, we return the *longer* of the answers for the current
;; line and the preceding line. This addresses the case of:
;;
;;   let x = 5 in
;;     nested block
;;   mis-indented line
;;
;; Which allows us to indent the mis-indented line forward during the
;; indent cycle.
;;
;; Unfortunately, layout means that we don't have close braces to work
;; with, so we usually need to parse forward from the beginning of the
;; current construct.
;;
;; For the moment, we build a list whose elements are of the form:
;; (position, token, actual-indent-level). That gives us enough that
;; we can make a second pass and canonicalize the indentation later,
;; which will give us what we need for indent-retion.
(defun bitc-observed-indent-stops ()
  "Computes the indent stops that are observed to be in effect at
`point'.

If point lies in the left-side white space of a line, actually
returns the indent stops observed to be in effect at the first
character of the first following token.

If point lies somewhere within a content line, the indent stops
returned reflect all indent stops that have been introduced to the
left of point."
  (interactive)
  (save-excursion
    (bitc-showme "Observed indent stops: %S"
      (let* (;; We're going to parse from form-start to
             ;; scan-limit. Scan-limit isn't quite b-o-l because
             ;; skip-forward-comment consumes leading and trailing white
             ;; space.
             (scan-limit (progn (skip-chars-forward " ")
                                (point)))
             ;; Note that the move to beginning-of-form here is a
             ;; desired result:
             (form-start (progn (bitc-beginning-of-form)
                                (point)))
             (indent-list nil))

        (defun get-token-end (tok)
          (bitc-get-token-end tok))
        (defun get-token-start (tok)
          (bitc-get-token-start tok))
        (defun get-token-col (tok)
          (bitc-get-token-col tok))
        (defun get-token-str (tok)
          (bitc-get-token-str tok))

        (defun add-indent (str col)
          (setq indent-list (cons `(,str ,col) indent-list)))

        ;; This is mildly tricky. It is called when we have seen a
        ;; token that implies a mandatory block. The next token
        ;; defines the block indent **unless* it is a '{', in which
        ;; case the token after *that* will define the block indent
        ;; and we defer adding the indent until we process the '{'
        (defun add-next-indent (str skip-over-curly)
          (when (not (stringp str))
            (setq str (bitc-get-token-str str)))

          (let ((next-tok (bitc-peek-next-token))
                (top-indent (car indent-list)))
            (cond
                ;; at EOF or outdent, use suggested indent:
                ((or (null next-tok)
                     (and top-indent 
                          (<= (get-token-col next-tok)
                             (nth 1 top-indent))))
                 (add-indent str
                             (+ (nth 1 (car indent-list))
                                (bitc-relative-indent tok))))

                ;; If next thing is an open curly and we're supposed
                ;; to defer to it:
                ((and skip-over-curly
                      (equal (get-token-str next-tok) "{"))
                 nil)                   ; do nothing

                (t ;; otherwise, add new indent entry:
                 (add-indent str
                             (get-token-col next-tok))))))

        (defvar last-removed-indent nil)
        (defun remove-indent ()
          (when indent-list
            (setq last-removed-indent (car indent-list))
            (setq indent-list (cdr indent-list))))

        (defun clear-last-removed-indent ()
          (setq last-removed-indent nil))

        (defun restore-last-indent ()
          (when last-removed-indent
            (setq indent-list (cons last-removed-indent indent-list))
            (clear-last-removed-indent)))

        (defun at-alignment-indent ()
          (and indent-list
               (let ((str (caar indent-list)))
                 (string-match "-align" str))))

        (defun continuation-tokenp (tok)
          (string-match bitc-continuation-tokens-re (get-token-str tok)))

        (defun top-form-tokenp (tok)
          (string-match bitc-top-form-names-re (get-token-str tok)))


        ;; Go all the way back to start of module to determine what
        ;; the baseline indent level is:
        (add-indent "<UOC>" 0)

        ;; We are hoping that the next token is "module" or
        ;; "interface".  In that case, we have an explicit unit of
        ;; compilation, and we should start processing there in order
        ;; to collect the associated indentation information. We'll
        ;; skip ahead after we collect the indent information for the
        ;; first "is" or "{".
        ;;
        ;; If this is *not* a module or interface, no point wasting
        ;; our time on the leading part of the file:

        (goto-char 0)
        ;; Skip any initial bitc-version:
        (forward-comment (buffer-size))
        (when (looking-at bitc-version-number-re)
          (goto-char (match-end))
          (forward-comment (buffer-size)))

        (unless (looking-at bitc-uoc-form-re)
          (goto-char form-start))

        ;;(let* ((tok (bitc-next-token)))
        ;;  (add-indent (get-token-str tok) (get-token-col tok)))

        (while (and (<= (point) scan-limit)
                    (< (point) (buffer-size)))
          (let* ((here (point))
                 (tok (bitc-next-token))
                 (tok-str (get-token-str tok))
                 (tok-col (get-token-col tok)))

            (when (equal (bitc-get-token-str tok) "<EOF>")
              (debug))
            (clear-last-removed-indent)

            ;; If this is an out-dent, the layout rule is to kill all
            ;; implicit blocks that are indented relative to this
            ;; token:
            (while (and indent-list
                        (not (equal (nth 0 (car indent-list)) "{"))
                        (< tok-col (nth 1 (car indent-list))))
              (remove-indent))

            ;; For indentation purposes, that isn't quite what
            ;; we want. The contrary case is:
            ;;
            ;;   let x = 5
            ;;   in
            ;;     blah
            ;;   .new-line
            ;;
            ;; That is: where `here' points to the first token of the
            ;; line following an indented block, and that first-token
            ;; is *not* a continuation token. In that case, we want
            ;; to offer the possibility to move the line right into
            ;; the block, which is the last indent that we just
            ;; deleted. So put it back:
            ;; (when (and (= (get-token-start tok) here)
            ;;            (not (continuation-tokenp tok))
            ;;            (not (top-form-tokenp tok)))
            ;;   (restore-last-indent))

            ;; If the present token is at the current indent, and it
            ;; is not a continuation token, and the present indent is
            ;; an alignment indent, remove the alignment indent:
            (while (and indent-list
                        (at-alignment-indent)
                        (not (continuation-tokenp tok))
                        (= tok-col (nth 1 (car indent-list))))
              (remove-indent))

            ;; If the next token is exactly at the scan limit, do not
            ;; add any indents that are introduced by the next token:
            (when (< here scan-limit)

              (cond
               ((or (equal tok-str "let")  
                    (equal tok-str "letrec"))
                ;; "fake" indent to support stand-alone "in":
                (add-indent "in-align" tok-col)
                (add-next-indent tok t))

               ((equal tok-str "in")
                (while (and indent-list
                            (not (equal (caar indent-list) "in-align")))
                  (remove-indent))
                (remove-indent)           ;the in-align
                (add-next-indent tok t))

               ((equal tok-str "loop")
                (add-indent "in-align" tok-col)
                (add-indent "until-align" tok-col)
                ; Following is wrong, but we need *something*:
                (add-indent "then-align" tok-col)
                (add-next-indent tok t))

               ((equal tok-str "until")
                ;; kill back to and including, the if-align:
                (while (and indent-list
                            (not (equal (caar indent-list) "until-align")))
                  (remove-indent))
                (remove-indent)           ;the until-align
                (add-next-indent tok t))

               ((equal tok-str "if")  
                ;; "fake" indent to support stand-alone "else, then":
                (add-indent "else-align" tok-col)
                (add-indent "then-align" tok-col)
                (add-next-indent tok t))

               ((equal tok-str "then")
                ;; kill back to and including, the if-align:
                (while (and indent-list
                            (not (equal (caar indent-list) "then-align")))
                  (remove-indent))
                (remove-indent)           ;the if-align
                (add-next-indent tok t))

               ((equal tok-str "else")
                ;; kill back to and including the if-align:
                (while (and indent-list
                            (not (equal (caar indent-list) "else-align")))
                  (remove-indent))
                (remove-indent)           ;the if-align
                (add-next-indent tok t))

               ((or (equal tok-str "try")  
                    (equal tok-str "switch"))
                ;; "fake" indent to support stand-alone "catch, case, otherwise":
                (add-indent "otherwise-align" tok-col)
                (add-indent "case-align" tok-col)
                (when (equal tok-str "try")
                  (add-indent "catch-align" tok-col))
                (add-next-indent tok t))

               ((equal tok-str "catch")
                ;; kill back to and including the case-align:
                (while (and indent-list
                            (not (equal (caar indent-list) "case-align")))
                  (remove-indent))
                (remove-indent)
                (add-next-indent tok t))

               ((equal tok-str "case")
                ;; kill back to, but not including, the case-align -
                ;; there could be multiple cases
                (while (and indent-list
                            (not (equal (caar indent-list) "case-align")))
                  (remove-indent))
                (add-next-indent tok t))

               ((equal tok-str "otherwise")
                ;; kill back to and including the otherwise-align:
                (while (and indent-list
                            (not (equal (caar indent-list) "otherwise-align")))
                  (remove-indent))
                (remove-indent)
                (add-next-indent tok t))

               ((equal tok-str "is")
                (while (and indent-list
                            (not (equal (caar indent-list) "is-align")))
                  (remove-indent))
                (remove-indent)          ;the is-align
                (add-next-indent tok t)

                ;; We may have been processing the initial
                ;; module/interface in order to collect the outermost
                ;; prevailing indent. If so, skip ahead to the start
                ;; of our intended form:
                (when (< here form-start)
                  (goto-char form-start))
                )

               ((or (equal tok-str "module")
                    (equal tok-str "interface"))
                (add-indent "is-align" tok-col)
                )

               ((equal tok-str "bitc")
                (add-indent tok-str tok-col))

              ((equal tok-str "def")
               (add-indent "in-align" tok-col)
               )

              ((or (equal tok-str "unboxed")
                   (equal tok-str "boxed")
                   (equal tok-str "def")
                   (equal tok-str "struct")
                   (equal tok-str "union")
                   (equal tok-str "repr")
                   (equal tok-str "trait")
                   (equal tok-str "instance")
                   (equal tok-str "exception"))
               (add-indent "is-align" tok-col)
               )

               ((equal tok-str "throw")
                (add-next-indent tok t))

               ((or (equal tok-str "{")
                    (equal tok-str "(")
                    (equal tok-str "["))
                (add-next-indent tok t))

               ((equal tok-str "}")
                (while (and indent-list
                            (not (equal (car (car indent-list)) "{")))
                  (remove-indent))
                (remove-indent)
                ;; We may have been processing the initial
                ;; module/interface in order to collect the outermost
                ;; prevailing indent. If so, skip ahead to the start
                ;; of our intended form:
                (when (< here form-start)
                  (goto-char form-start))
                )

               ((equal tok-str ")")
                (while (and indent-list
                            (not (equal (caar indent-list) "(")))
                  (remove-indent))
                (remove-indent))

               ((equal tok-str "]")
                (while (and indent-list
                            (not (equal (caar indent-list) "[")))
                  (remove-indent))
                (remove-indent))

               ;; Introduce tab-stop after that assignments line up.
               ((equal tok-str ":=")
                (add-next-indent tok t))

               ;; Introduce tab-stop after that initializations line
               ;; up. Additionally, if the top indent is an in-align,
               ;; then we are looking at the binding of a def form,
               ;; and we should drop the in-align:
               ((equal tok-str "=")
                (add-next-indent tok t)
                (when (equal (caar indent-list) "in-align")
                  (remove-indent)))

               ;; We do not introduce any tab stop for ",", because
               ;; all legal "," tokens appear within some form of
               ;; brackets, and we have already introduced tab-stops
               ;; for these above.

               ) ;;end of cond for current token

              (forward-comment (buffer-size)))))

        ;; There are some cases that we handle by looking ahead to
        ;; the next token EVEN if it lies beyond point. The
        ;; problem cases mainly have to do with continuation
        ;; alignment. Given:
        ;;
        ;;   let x = 5
        ;; .
        ;;   in
        ;;
        ;; we are looking to add a new binding, and the reported
        ;; indent should be the indent for the LET. But given:
        ;;
        ;;   let x = 5
        ;; . in
        ;;
        ;; we are being asked to re-indent the "in", and the
        ;; reported indent should therefore be the in-align
        ;; indent:

        ;; (let ((next-tok (bitc-peek-next-token)))
        ;;   (when (and (not (looking-at "[ \t]*\n"))
        ;;              (or (continuation-tokenp next-tok)
        ;;                  (top-form-tokenp tok)))
        ;;     (when (equal (car next-tok) "in")
        ;;       (debug))
        ;;     (while (and indent-list
        ;;                 (not (at-alignment-indent)))
        ;;       (remove-indent))))

        indent-list))))

(defun bitc-canonical-indent-stops ()
  "Given a set of observed indent stops, returns the canonical
indents according to the prevailing mode indent rules."
  (interactive)

  (bitc-showme "Canonical indent list: %S"
    (defun canonicalize-indent (indent adjust)
      (let* ((stop (cadr indent))
             (base (+ stop adjust))
             (s (car indent)))

        (cond ((or (equal s "module")
                   (equal s "interface")
                   (equal s "bitc"))
               (cons s 0))
              ((or (equal s "unboxed")
                   (equal s "boxed")
                   (equal s "def")
                   (equal s "struct")
                   (equal s "union")
                   (equal s "repr")
                   (equal s "trait")
                   (equal s "instance")
                   (equal s "exception")
               (cons s 0))
              (t (cons s base)))))
    

    (defun canonicalize (lst out adjust)
      (if (null lst) out
        (let* ((cur-indent (car lst))
               (new-indent (canonicalize-ident cur-indent adjust))
               (adjust (- cadr new-indent) (cadr cur-indent)))
          (canonicalize (cdr lst)
                        (cons new-indent out)
                        adjust))))
    
    (let* ((stops (nreverse (bitc-observed-indent-stops))))
      (canonicalize stops nil 0)))))

(defun bitc-indent-stops ()
  "This is a placeholder wrapper for later, when I attempt to compute
canonical indents."
  (interactive)
  (bitc-showme "Indent stops %S"
    (bitc-observed-indent-stops)))

(defvar bitc-xcycle-indent-info nil
  "Indentation cycle information for the current line, as originally
computed.

This is used to support indent cycling. The current state is
wiped whenever indent is re-computed for a new line, or when
electric indent happens.

This is a tuple of the form

  (first-line-position nlines char-mod-tick a-list)

where `a-list'
elements take the form `(current-indent next-cycle-indent)'. The first
element of the a-list should be used when there is no match.

When the current indent is unambiguous, the a-list will have a
single element.")

(defun bitc-clear-cycle-state ()
  (setq bitc-xcycle-indent-info  nil))

(defun bitc-compute-indent-info ()
  (interactive)
  (bitc-maybe-clear-cycle-state)

  (defun do-make-cycle (lst stops first)
    (if (> (length stops) 1)
        (if (= (car stops) (cadr stops))
            ;; Don't add positions redundantly
            (do-make-cycle lst (cdr stops) first)
          (do-make-cycle (cons `(,(car stops) ,(cadr stops)) lst)
                         (cdr stops)
                         first))
      (cons `(,(car stops) ,first) lst)))

  (defun cycle-lines (start)
    (save-excursion
      (goto-char start)
      (let ((col (bitc-current-line-current-indent))
            (nlines 1))
        (forward-line 1)
        (while (and (not (eobp))
                    (or (> (bitc-current-line-current-indent) col)
                        (bitc-current-line-blankp)))
          (setq nlines (+ nlines 1))
          (forward-line 1))
        nlines)))

  (defun bitc-make-cycle (bol &rest stops)
    "Given the position of the first line `bol', and a set of
tabstops `stops', return a suitable indent cycle structure."

    (when (null stops)
      (debug))

    (list bol (cycle-lines bol)
          (buffer-chars-modified-tick)
          (nreverse (do-make-cycle nil stops (car stops)))))

  (bitc-showme "Indent cycle is %S"
    (save-excursion
      (beginning-of-line)
      (let ((bol (point)))
        (when (null bitc-xcycle-indent-info)
          ;; While I have added some heuristics, this could be
          ;; improved. For example, if we have a "{" stop, and we
          ;; aren't looking at "}", we should remove everything to the
          ;; left of the "{" stop. If we *are* looking at "{", we
          ;; should remove that much less one.
          (let ((stops (bitc-indent-stops)))
            (cond ((or (equal (caar stops) "=")
                       (equal (caar stops) ":="))
                   ;; Rotate the assignment/binding tab stop back one in
                   ;; the list:
                   (setq stops (cons (cadr stops)
                                     (cons (car stops)
                                           (cddr stops)))))
                  ((bitc-continuation-linep)
                   (let* ((tok-str (match-string 0))
                          (align-marker (concat tok-str "-align")))
                     (while (and stops
                                 (not (equal (caar stops) align-marker)))
                       (setq stops (cdr stops)))
                     (setq stops (list (car stops)))))
                  

                  )
            (setq bitc-xcycle-indent-info
                  (apply 'bitc-make-cycle 
                         (cons bol
                               (mapcar (lambda (x) (cadr x))
                                       stops)))))))
      bitc-xcycle-indent-info)))

(defun bitc-cycle-bol (info)
  (nth 0 info))

(defun bitc-cycle-nlines (info)
  (nth 1 info))

(defun bitc-cycle-tick (info)
  (nth 2 info))

(defun bitc-cycle-stops (info)
  (nth 3 info))

(defun bitc-cycle-update-modtick ()
  (let ((info bitc-xcycle-indent-info))
    (setq bitc-xcycle-indent-info
          (list (bitc-cycle-bol info)
                (bitc-cycle-nlines info)
                (buffer-chars-modified-tick)
                (bitc-cycle-stops info)))))

(defun bitc-get-cycle-info ()
  (defun bitc-maybe-clear-cycle-state ()
    (when (consp bitc-xcycle-indent-info)
      (save-excursion
        (beginning-of-line)
        (let ((bol (point))
              (cmod-tick (buffer-chars-modified-tick))
              (info bitc-xcycle-indent-info))
          (when (or (not (= bol (bitc-cycle-bol info)))
                    (not (= cmod-tick (bitc-cycle-tick info))))
            (bitc-clear-cycle-state))))))

  (bitc-maybe-clear-cycle-state)
  (when (null bitc-xcycle-indent-info)
    (bitc-compute-indent-info))
  bitc-xcycle-indent-info)    

(defun bitc-indent-cycle-to (col)
  "Indent the current indent cycle region to begin at `col'. Assumes
that point lies within the first line of the cycle. If the line
containing point is blank, only that line is re-indented."
  (save-excursion
    (beginning-of-line)
    (let* ((info (bitc-get-cycle-info))
           (cur-col (bitc-current-line-current-indent))
           (delta (- col cur-col))
           (nlines (bitc-cycle-nlines info)))
      ;; If we are asked to indent a currently blank line, it is very
      ;; likely a line introduced by the user typing RET following an
      ;; opening curly brace or some other block-introducing form. In
      ;; this caes we don't want to do any indent adjustment on
      ;; subsequent lines (which may or may not be part of this
      ;; block).
      (if (looking-at "^[ \t]*$")
          (indent-line-to col)
        (while (and (not (eobp))
                    (> nlines 0))
          (if (looking-at "[ \t]*$") (indent-line-to 0)
            (indent-line-to (+ (bitc-current-line-current-indent) delta)))
          (forward-line 1)
          (setq nlines (- nlines 1)))))))

(defun bitc-cycle-indent (current-indent)
  "Cycle the user through the available indent stops at the current line."
  (let* ((info (bitc-get-cycle-info))
         (a-list (bitc-cycle-stops info))
         (cycle (assoc current-indent a-list))
         (default-indent (caar a-list)))      ;default
    (message "Info is %S" a-list)
    (message "A-list is %S" a-list)
    (message "cycle is %S" cycle)
    (message "default indent is %S" default-indent)
    (if (and (memq last-command '(indent-for-tab-command newline-and-indent))
             (not (null cycle)))
        (setq default-indent (cadr cycle)))

    (bitc-indent-cycle-to default-indent)
    (bitc-cycle-update-modtick)))


(defun bitc-comment-linep ()
  "Return t if the current line begins with a comment."
  (interactive)
  (bitc-showme "Comment line: %S"
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t")
      (or (nth 4 (syntax-ppss))
          (looking-at bitc-comment-re)))))

(defun bitc-show-prefix (arg)
  (interactive "p")
  (bitc-showme "Prefix arg: %S" arg))

(defun bitc-delete-backward-char (arg &optional killp)
  (interactive "p")
  (let ((here (point)))
    (if (or (> arg 1)
            (= here (bitc-beginning-of-current-line))
            (not (bitc-in-left-indent-areap here t)))
      (backward-delete-char-untabify arg killp)
    (progn
      (bitc-maybe-clear-cycle-state)
      (let ((stops (bitc-indent-stops))
            (cur-indent (bitc-current-line-current-indent)))
        (while (and stops 
                    (>= (cadr (car stops)) cur-indent))
          (setq stops (cdr stops)))
        (if stops
            (bitc-indent-cycle-to (cadr (car stops)))
          (backward-delete-char-untabify arg killp)))))))

(defun bitc-delete-char (n &optional killflag)
  (interactive "p")
  (let ((here (point)))
    (if (or (> n 1)
            (not (bitc-in-left-indent-areap here nil)))
      (delete-char n killflag)
    (progn
      (bitc-maybe-clear-cycle-state)
      (let ((stops (bitc-indent-stops))
            (cur-indent (bitc-current-line-current-indent)))
        (while (and stops 
                    (>= (cadr (car stops)) cur-indent))
          (setq stops (cdr stops)))
        (if stops
            (bitc-indent-cycle-to (cadr (car stops)))
          (delete-char n killflag)))))))

(defun bitc-point-within-comment-linep ()
  (interactive)
  (bitc-showme "Point in comment-line: %S"
    (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (or (nth 4 (syntax-ppss))
        (looking-at bitc-comment-re)))))

(defun bitc-end-of-comment ()
  (interactive)
  (parse-partial-sexp (point) (buffer-size)
                      nil
                      nil
                      (syntax-ppss)
                      'syntax-table))

(defun bitc-indent-new-comment-line (&optional soft)
  (interactive)
  (let ((in-block-comment (null (nth 7 (syntax-ppss)))))
    
  (delete-horizontal-space)
  (newline nil)                         ;suppress recursive auto-fill
  (if in-block-comment (insert " * ")
    (insert comment-start))
  (bitc-indent-comment-line)
  (end-of-line)
  ))

(defun bitc-indent-comment-line ()
  (interactive)
  (let ((indent
         (save-excursion
           (beginning-of-line)
           (skip-chars-forward " \t")

           ;; Inside a block comment. Find the indent of the next
           ;; non-comment line:
           (while (nth 4 (syntax-ppss))
             (re-search-backward "/\\*" nil 'move))

           (forward-comment (buffer-size))
           (bitc-current-line-current-indent))))
    
    (beginning-of-line)
    (skip-chars-forward " \t")
    (when (not (looking-at bitc-comment-re))
      ;; If we aren't looking at start of comment, then the current
      ;; line must be a comment continuation line on a block comment:
      (setq indent (+ indent 1)))
    (indent-line-to indent)))

(defun bitc-indent-line ()
 (interactive)
 (if (bitc-point-within-comment-linep)
     (bitc-indent-comment-line)
   (bitc-cycle-indent (bitc-current-line-current-indent))))

(defun bitc-newline-and-indent ()
  (interactive)
  (if (nth 4 (syntax-ppss))
      (bitc-indent-new-comment-line)
    (newline-and-indent)))

(provide 'bitc-parse)

;;; bitc-parse.el ends here
