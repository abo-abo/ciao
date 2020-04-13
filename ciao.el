;;; ciao.el --- Smart lexeme navigation for the C family. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ciao
;; Version: 0.1.0
;; Package-Requires: ((lispy "0.23.0"))
;; Keywords: C, C++

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;; Code:

(require 'lpy)
(require 'cc-mode)

(defgroup ciao nil
  "Smart lexeme navigation for the C family."
  :group 'bindings
  :prefix "ciao-")

(defvar ciao-mode-map (make-sparse-keymap)
  "Keymap for `ciao-mode'.")

(defconst ciao-outline-regexp "^//\\*+")

(defconst ciao-font-lock-keywords
  '(("^\\(//\\* .*\\)$" 1 'org-level-1 prepend)
    ("^\\(//\\*\\* .*\\)$" 1 'org-level-2 prepend)
    ("^\\(//\\*\\*\\* .*\\)$" 1 'org-level-3 prepend)
    ("^\\(//\\*\\*\\*\\* .*\\)$" 1 'org-level-4 prepend)
    ("^\\(//\\*\\*\\*\\*\\*+ .*\\)$" 1 'org-level-5 prepend)
    ("`\\([^\n']+\\)'" 1 font-lock-constant-face prepend)))

(define-minor-mode ciao-mode
  "Navigating C/C++ with plain keys.

\\{ciao-mode-map}"
  :keymap ciao-mode-map
  :group 'ciao
  :lighter " ;)"
  (if ciao-mode
      (progn
        (setq-local outline-level 'ciao-outline-level)
        (setq-local outline-regexp (substring ciao-outline-regexp 1))
        (setq-local outline-heading-end-regexp "\n")
        (setq-local lispy-outline ciao-outline-regexp)
        (setq lispy-outline-header "//")
        (font-lock-add-keywords major-mode ciao-font-lock-keywords))
    (font-lock-remove-keywords major-mode ciao-font-lock-keywords)))

(defun ciao-forward (arg)
  (interactive "p")
  (let ((pt (point))
        (r (lispy-dotimes arg
             (when (= (point) (point-max))
               (error "Reached end of buffer"))
             (forward-list))))
    (if (or (null r)
            (= pt (point)))
        (prog1 nil
          (ciao-out-forward 1))
      (point))))

(defun ciao-backward (arg)
  (interactive "p")
  (if (and (lispy--in-comment-p)
           (save-excursion
             (move-beginning-of-line 1)
             (ciao--outlinep)))
      (move-beginning-of-line 1)
    (let ((pt (point))
          (r (lispy-dotimes arg
               (when (= (point) (point-min))
                 (error "Reached beginning of buffer"))
               (backward-list))))
      (if (or (null r)
              (= pt (point)))
          (prog1 nil
            (condition-case nil
                (progn
                  (ciao-out-forward 1)
                  (backward-list))
              (error
               (goto-char pt))))
        (point)))))

(defun ciao-out-forward (arg)
  (interactive "p")
  (lispy-dotimes arg
    (let ((s (syntax-ppss)))
      (when (nth 3 s)
        (goto-char (nth 8 s)))
      (up-list))))

(defun ciao-flow--forward ()
  (forward-char)
  (re-search-forward "(\\|{" nil t)
  (while (and (lispy--in-string-or-comment-p)
              (re-search-forward "(\\|{" nil t)))
  (backward-char))

(defun ciao--commentp ()
  (or (looking-at "/[/*]")
      (lispy-after-string-p "*/")))

(defun ciao-outline-level ()
  "Compute the outline level of the heading at point."
  (save-excursion
    (save-match-data
      (end-of-line)
      (if (re-search-backward ciao-outline-regexp nil t)
          (max (cl-count ?* (match-string 0)) 1)
        0))))

(defun ciao-tab ()
  (interactive)
  (when (ciao--outlinep)
    (let ((org-outline-regexp outline-regexp))
      (org-cycle-internal-local))))

(defun ciao-shifttab (arg)
  "Hide/show outline summary.
When ARG isn't nil, show table of contents."
  (interactive "P")
  (require 'org)
  (outline-minor-mode 1)
  (let ((org-outline-regexp outline-regexp))
    (lispy-flet (org-unlogged-message (&rest _x))
      (if arg
          (org-content)
        (when (eq org-cycle-global-status 'overview)
          (setq org-cycle-global-status 'contents))
        (org-cycle-internal-global))))
  (recenter))

(defun ciao-flow--backward ()
  (backward-char)
  (re-search-backward ")\\|}" nil t)
  (while (and (lispy--in-string-or-comment-p)
              (re-search-backward ")\\|}" nil t)))
  (forward-char))

(defun ciao-flow ()
  (interactive)
  (cond ((looking-at "\\s(")
         (ciao-flow--forward))

        ((looking-back ")\\|\\}")
         (ciao-flow--backward))

        (t
         (cond ((looking-back "= ")
                (backward-delete-char 1)
                (insert "= "))

               ((looking-back " ")
                (insert "= "))

               (t (insert " = "))))))

(defun ciao-mark-line ()
  "Mark the current line."
  (interactive)
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (set-mark beg)
    (goto-char end)))

(defun ciao-mark-symbol ()
  "Mark the current symbol."
  (interactive)
  (cond ((looking-back "^")
         (lispy--mark
          (cons (line-beginning-position)
                (line-end-position))))
        (t
         (lispy-mark-symbol))))

(defun ciao-linep ()
  (and (region-active-p)
       (let ((rb (region-beginning))
             (re (region-end)))
         (save-excursion
           (and (= (goto-char rb) (line-beginning-position))
                (= (goto-char re) (line-end-position)))))))

(defun ciao-slurp ()
  (interactive)
  (cond ((ciao-linep)
         (if (= (point) (region-end))
             (move-end-of-line 2)
           (move-beginning-of-line 0)))
        ((region-active-p)
         (if (eq (point) (region-end))
             (progn
               (forward-sexp 1)
               (when (looking-at ";\n")
                 (forward-char 1)))
           (forward-sexp -1)))
        (t
         (soap-command))))

(defun ciao-barf ()
  (interactive)
  (cond ((ciao-linep)
         (if (= (point) (region-end))
             (unless (= (region-beginning) (line-beginning-position))
               (move-end-of-line 0))
           (unless (= (region-end) (line-end-position))
             (move-beginning-of-line 2))))
        ((region-active-p)
         (if (eq (point) (region-beginning))
             (progn
               (forward-sexp 1)
               (when (looking-at ";\n")
                 (forward-char 1)))
           (forward-sexp -1)))
        ((lispy--in-string-or-comment-p)
         (call-interactively 'self-insert-command))
        (t
         (soap-command))))

(defun ciao-parent-end-position ()
  (save-excursion
    (backward-up-list)
    (forward-sexp)
    (point)))

(defun ciao-parent-beg-position ()
  (save-excursion
    (backward-up-list)
    (point)))

(defun ciao-next-top-level-sexp ()
  (forward-char 1)
  (re-search-forward "^[^ \n}]" (lispy--outline-end) t)
  (forward-char -1))

(defun ciao-down (arg)
  (interactive "p")
  (cond ((lpy-line-left-p)
         (let ((region (region-active-p)))
           (deactivate-mark)
           (if (bolp)
               (ciao-next-top-level-sexp)
             (let ((end (ciao-parent-end-position))
                   (indent (buffer-substring-no-properties
                            (line-beginning-position)
                            (1+ (point)))))
               (when (re-search-forward (concat "^" indent "\\b") end t)
                 (backward-char))))
           (when region
             (ciao-mark))))
        ((ciao--outlinep)
         (let ((pt (point)))
           (lispy-dotimes arg
             (outline-next-visible-heading 1)
             (if (ciao--outlinep)
                 (setq pt (point))
               (goto-char pt)
               (error "Last outline reached")))))))

(defun ciao-up (arg)
  (interactive "p")
  (cond ((lpy-line-left-p)
         (let ((region (region-active-p)))
           (deactivate-mark)
           (if (bolp)
               (re-search-backward "^[^ \n}]" (lispy--outline-beg) t)
             (let ((beg (ciao-parent-beg-position))
                   (indent (buffer-substring-no-properties
                            (line-beginning-position)
                            (1+ (point)))))
               (when (re-search-backward (concat "^" indent "\\b") beg t)
                 (back-to-indentation)
                 (backward-char))))
           (when region
             (ciao-mark))))
        ((ciao--outlinep)
         (let ((pt (point)))
           (lispy-dotimes arg
             (outline-previous-visible-heading 1)
             (if (ciao--outlinep)
                 (setq pt (point))
               (goto-char pt)
               (error "First outline reached")))))))

(defun ciao-left ()
  (interactive)
  (cond ((and (region-active-p)
              (save-excursion
                (goto-char (region-beginning))
                (ciao-leftp)))
         (let ((rb (region-beginning))
               (re (region-end)))
           (goto-char re)
           (move-end-of-line 1)
           (set-mark (point))
           (goto-char rb)
           (move-beginning-of-line 1)))
        ((looking-at lispy-outline)
         (lispy-outline-left))
        (t
         (unless (bolp)
           (let ((pt (point)))
             (condition-case nil
                 (progn
                   (backward-up-list)
                   (back-to-indentation)
                   (unless (bolp)
                     (backward-char)))
               (error (goto-char pt))))))))

(defun ciao-right ()
  (interactive)
  (cond ((region-active-p)
         (ciao-left)
         (exchange-point-and-mark))
        ((looking-at lispy-outline)
         (lispy-outline-right))
        ((lpy-line-left-p)
         (let* ((cur-offset (current-column))
                (new-offset (+ cur-offset c-basic-offset))
                (regex (concat "^" (make-string new-offset ?\ ))))
           (when (re-search-forward regex nil t)
             (backward-char))))))

(defun ciao-to ()
  (interactive)
  (cond ((ciao--outlinep)
         (end-of-line)
         (unless (looking-back " ")
           (insert " ")))))

(defsubst ciao-leftp ()
  (looking-at "[[{(]"))

(defun ciao-exprp ()
  (unless (eolp)
    (or (bolp)
        (and (looking-at " ")
             (looking-back "^ *" (line-beginning-position))))))

(defsubst ciao-rightp ()
  (looking-back "[]})]"))

(defun ciao-different ()
  (interactive)
  (cond ((region-active-p)
         (exchange-point-and-mark))
        ((ciao-leftp)
         (forward-list))
        ((ciao-rightp)
         (backward-list))))

(require 'subr-x)

(defun ciao-delete (arg)
  (interactive "p")
  (if (region-active-p)
      (delete-region
       (region-beginning)
       (region-end))
    (c-electric-delete-forward 1)))

(defun ciao-eval ()
  (interactive)
  (require 'cmacexp)
  (when (region-active-p)
    (let ((expansion
           (let ((inhibit-message t))
             (setq expansion
                   (c-macro-expansion
                    (region-beginning) (region-end)
                    (concat c-macro-preprocessor " "
                            c-macro-cppflags) t)))))
      (message (string-trim expansion)))))

(defun ciao-mark ()
  (interactive)
  (cond ((region-active-p)
         (deactivate-mark))
        ((ciao-leftp)
         (lispy--mark (lispy--bounds-dwim)))
        ((ciao-rightp)
         (lispy--mark (lispy--bounds-dwim))
         (exchange-point-and-mark))))

(defun ciao--outlinep ()
  (looking-at "//\\*"))

(defun ciao--insert-or-call (def plist)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'."
  `(lambda ,(help-function-arglist def)
     ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
              (symbol-name def) (documentation def))
     ,(interactive-form def)
     (cond ((region-active-p)
            (call-interactively ',def))
           ((lispy--in-string-or-comment-p)

            (call-interactively ',(or (plist-get plist :self-insert) 'self-insert-command)))

           ((or
             (ciao-exprp)
             (ciao-leftp)
             (ciao-rightp)
             (and (looking-back "^ *")
                  (looking-at "//")))
            (call-interactively ',def))
           (t
            (call-interactively ',(or (plist-get plist :self-insert) 'self-insert-command))))))

(defun ciao-define-key (keymap key def &rest plist)
  "Forward to (`define-key' KEYMAP KEY FUNC)."
  (declare (indent 3))
  (let ((func (defalias (intern (concat "cspecial-" (symbol-name def)))
                  (ciao--insert-or-call def plist))))
    (add-to-list 'ac-trigger-commands func)
    (unless (memq func mc/cmds-to-run-once)
      (add-to-list 'mc/cmds-to-run-for-all func))
    (define-key keymap (kbd key) func)))

(defun ciao-spacify ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "(" nil t)
      (backward-char 1)
      (unless (lispy--in-string-or-comment-p)
        (just-one-space))
      (forward-char 1))))

(defun ciao-parens (&optional arg)
  (interactive "P")
  (cond ((region-active-p)
         (lispy--surround-region "(" ")"))
        (arg
         (let ((bnd (lispy--bounds-dwim)))
           (goto-char (cdr bnd))
           (insert ")")
           (save-excursion
             (goto-char (car bnd))
             (insert "("))))
        (t
         (cond
           ((lispy--in-string-p))
           ((lispy--in-comment-p))
           ((lispy-after-string-p "("))
           ((lispy-after-string-p "["))
           ((lispy-after-string-p "*")
            (delete-char -1)
            (insert " * "))
           ((looking-back "^ *" (line-beginning-position)))
           ((looking-back "^#define *[A-Z\\|_0-9]+" (line-beginning-position)))
           (t
            (just-one-space)))
         (insert "()")
         (backward-char))))

(defun ciao-paste (arg)
  (interactive "p")
  (cond ((region-active-p)
         (let ((bnd (lispy--bounds-dwim)))
           (deactivate-mark)
           (delete-region (car bnd)
                          (cdr bnd))
           (yank)))))

(defhydra hydra-ciao-goto (:color blue
                           :pre (setq hydra-lv nil)
                           :before-exit (setq hydra-lv t))
  "goto"
  ("g" moo-jump-local)
  ("o" ciao-goto-outline)
  ("l" avy-goto-line))

(defun ciao--outlines ()
  (let (res)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward outline-regexp nil t)
        (let ((str (buffer-substring
                    (+ 2 (line-beginning-position))
                    (line-end-position))))
          (put-text-property 0 1 'point (line-beginning-position) str)
          (push str res))))
    (nreverse res)))

(defun ciao-goto-outline-action (x)
  (with-ivy-window
    (goto-char (get-text-property 0 'point x))))

(defun ciao-goto-outline ()
  (interactive)
  (ivy-read "Goto outline: " (ciao--outlines)
            :action #'ciao-goto-outline-action))

(defun ciao-goto-symbol ()
  (interactive)
  (require 'semantic/ia)
  (deactivate-mark)
  (ring-insert
   find-tag-marker-ring
   (point-marker))
  (semantic-ia-fast-jump (point)))

(let ((map ciao-mode-map))
  (define-key map (kbd "φ") 'ciao-parens)
  (define-key map (kbd "]") 'ciao-forward)
  (define-key map (kbd "[") 'ciao-backward)
  (define-key map (kbd "χ") 'ciao-out-forward)
  (define-key map (kbd "M-l") 'ciao-mark-line)
  (define-key map (kbd "M-.") 'ciao-goto-symbol)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map (kbd "M-m") 'ciao-mark-symbol)
  (define-key map (kbd "M-RET") 'lispy-meta-return)
  (define-key map (kbd "<backtab>") 'ciao-shifttab)
  (define-key map (kbd "C-d") 'ciao-delete)
  (dolist (k '("+" "-" "%" "&" "|" "<" "=" ">" ","))
    (define-key map (kbd k) 'soap-command))
  (ciao-define-key map "b" 'lispy-back)
  (ciao-define-key map "i" 'ciao-tab)
  (ciao-define-key map "c" 'hydra-lispy-move/body)
  (ciao-define-key map "g" 'hydra-ciao-goto/body)
  (ciao-define-key map "j" 'ciao-down)
  (ciao-define-key map "k" 'ciao-up)
  (ciao-define-key map "h" 'ciao-left)
  (ciao-define-key map "l" 'ciao-right)
  (ciao-define-key map "d" 'ciao-different)
  (ciao-define-key map "m" 'ciao-mark)
  (ciao-define-key map ">" 'ciao-slurp :self-insert 'soap-command)
  (ciao-define-key map "<" 'ciao-barf :self-insert 'soap-command)
  (ciao-define-key map "e" 'ciao-eval)
  (ciao-define-key map "f" 'ciao-flow)
  (ciao-define-key map "t" 'ciao-to)
  (ciao-define-key map "x" 'lispy-x)
  (ciao-define-key map "P" 'ciao-paste)
  (ciao-define-key map "J" 'lispy-outline-next)
  (ciao-define-key map "K" 'lispy-outline-prev))

(provide 'ciao)

;;; ciao.el ends here
