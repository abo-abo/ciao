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

(require 'lispy)

(defgroup ciao nil
  "Smart lexeme navigation for the C family."
  :group 'bindings
  :prefix "ciao-")

(defvar ciao-mode-map (make-sparse-keymap)
  "Keymap for `ciao-mode'.")

(define-minor-mode ciao-mode
    "Navigating C/C++ is a piece of ciao.

\\{ciao-mode-map}"
  :keymap ciao-mode-map
  :group 'ciao
  :lighter " yum")

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
        ((looking-at "$")
         (lispy--mark
          (cons (line-beginning-position)
                (line-end-position)))
         (exchange-point-and-mark))
        (t
         (let ((bnd (bounds-of-thing-at-point 'sexp)))
           (lispy--mark bnd)))))

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
           (move-beginning-of-line 0)))))

(defun ciao-barf ()
  (interactive)
  (cond ((ciao-linep)
         (if (= (point) (region-end))
             (unless (= (region-beginning) (line-beginning-position))
               (move-end-of-line 0))
           (unless (= (region-end) (line-end-position))
             (move-beginning-of-line 2))))))

(defun ciao-down ()
  (interactive)
  (cond ((ciao-linep)
         (set-mark (1+ (line-end-position)))
         (move-end-of-line 2))))

(defun ciao-up ()
  (interactive)
  (cond ((ciao-linep)
         (move-beginning-of-line 0)
         (set-mark (point))
         (move-end-of-line 1))))

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
           (move-beginning-of-line 1)))))

(defun ciao-right ()
  (interactive)
  (cond ((region-active-p)
         (ciao-left)
         (exchange-point-and-mark))))

(defun ciao-delete-backward ()
  (interactive)
  (if (region-active-p)
      ()))

(defsubst ciao-leftp ()
  (looking-at "[{(]"))

(defsubst ciao-rightp ()
  (looking-back "[})]"))

(defun ciao-different ()
  (interactive)
  (cond ((region-active-p)
         (exchange-point-and-mark))
        ((ciao-leftp)
         (forward-list))
        ((ciao-rightp)
         (backward-list))))

(defun ciao-mark ()
  (interactive)
  (cond ((region-active-p)
         (deactivate-mark))
        ((ciao-leftp)
         (lispy--mark (lispy--bounds-dwim)))
        ((ciao-rightp)
         (lispy--mark (lispy--bounds-dwim))
         (exchange-point-and-mark))))

(defun ciao--insert-or-call (def)
  "Return a lambda to call DEF if position is special.
Otherwise call `self-insert-command'."
  `(lambda ,(help-function-arglist def)
     ,(format "Call `%s' when special, self-insert otherwise.\n\n%s"
              (symbol-name def) (documentation def))
     ,(interactive-form def)
     (cond ((region-active-p)
            (call-interactively ',def))
           ((lispy--in-string-or-comment-p)
            (call-interactively 'self-insert-command))

           ((or (ciao-leftp)
                (ciao-rightp)
                (and (looking-back "^ *")
                     (looking-at "//")))
            (call-interactively ',def))
           (t
            (call-interactively 'self-insert-command)))))

(defun ciao-define-key (keymap key def)
  "Forward to (`define-key' KEYMAP KEY FUNC)."
  (declare (indent 3))
  (let ((func (defalias (intern (concat "cspecial-" (symbol-name def)))
                  (ciao--insert-or-call def))))
    (add-to-list 'ac-trigger-commands func)
    (unless (memq func mc/cmds-to-run-once)
      (add-to-list 'mc/cmds-to-run-for-all func))
    (unless (memq func company-no-begin-commands)
      (add-to-list 'company-begin-commands func))
    (define-key keymap (kbd key) func)))

(let ((map ciao-mode-map))
  (define-key map (kbd "M-l") 'ciao-mark-line)
  (define-key map (kbd "M-m") 'ciao-mark-symbol)
  (ciao-define-key map "j" 'ciao-down)
  (ciao-define-key map "k" 'ciao-up)
  (ciao-define-key map "h" 'ciao-left)
  (ciao-define-key map "l" 'ciao-right)
  (ciao-define-key map "d" 'ciao-different)
  (ciao-define-key map "m" 'ciao-mark)
  (ciao-define-key map ">" 'ciao-slurp)
  (ciao-define-key map "<" 'ciao-barf))


(provide 'ciao)

;;; ciao.el ends here
