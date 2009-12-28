;;; init_python.el --- Python Setting

;; Copyright (C) 2004  sakito

;; Author: sakito <sakito@sakito.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary: Python-mode および Pythonコーディングに便利な設定

;; 

;;; Code:

;;; Python-mode
;; @see http://www.python.org/emacs/python-mode/
;;
(setenv "PYTHONSTARTUP" "~/.pythonrc.py")

(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cgi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(modify-coding-system-alist 'file "\\.py$" '(undecided . utf-8-unix))
(modify-coding-system-alist 'file "\\.cgi$" '(undecided . utf-8-unix))

(setq py-python-command "/usr/local/bin/python")

(add-hook 'python-mode-hook
          (lambda ()
            (require 'pycomplete)
            (setq indent-tabs-mode nil)
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")))

;; カッコの対応付け
;; @see http://www.emacswiki.org/cgi-bin/wiki/PythonMode#toc7
;;(add-hook 'python-mode-hook
;;              (lambda ()
;;                (define-key python-mode-map "\"" 'electric-pair)
;;                (define-key python-mode-map "\'" 'electric-pair)
;;                (define-key python-mode-map "(" 'electric-pair)
;;                (define-key python-mode-map "[" 'electric-pair)
;;                (define-key python-mode-map "{" 'electric-pair)))

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

;; pdb
;(setq pdb-path '/usr/lib/python2.5/pdb.py
;      gud-pdb-command-name (symbol-name pdb-path))
(setq pdb-path '/Library/Frameworks/Python.framework/Versions/2.6/lib/python2.6/pdb.py
      gud-pdb-command-name (symbol-name pdb-path))
(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

;; doctest
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(autoload 'doctest-mode
  "doctest-mode" "Editing mode for Python Doctest examples." t)

;; Pymacs
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; ipython
(require 'ipython)
(setq ipython-command "/usr/local/bin/ipython")
;; http://www.emacswiki.org/emacs/anything-ipython.el
;; http://www.emacswiki.org/emacs/anything-show-completion.el
(require 'anything-show-completion)
(require 'anything-ipython)
(add-to-list 'anything-sources 'anything-source-ipython)
(add-hook 'python-mode-hook #'(lambda ()
                                (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
(add-hook 'python-mode-hook #'(lambda ()
                                (define-key py-mode-map (kbd "C-c e") 'anything-ipython-complete)))
(add-hook 'ipython-shell-hook #'(lambda ()
                                  (define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)))
(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-ipython-complete
                                '(length initial-pattern)))


(eval-after-load "info-look"
  '(info-lookup-add-help
    :topic 'symbol
    :mode 'python-mode
    ;:regexp "[a-zA-Z0-9_]+"
    ;:regexp "^`[][a-zA-Z0-9_(),... ]+'$"
    :regexp "[][a-zA-Z0-9_(),... ]+"
    :ignore-case t
    :doc-spec '(
                ("(python-lib-ja)Module Index" nil "^`")
                ("(python-lib-ja)Class-Exception-Object Index"  nil "^`")
                ("(python-lib-ja)Function-Method-Variable Index"  nil "^`")
                ("(python-lib-ja)Miscellaneous Index"  nil "^`")
                )))

;(eval-after-load "info-look"
;  '(info-lookup-add-help
;    :mode 'python-mode
;    :regexp "\\([a-zA-Z0-9_-]+\\|[!{}@*#?$]\\|\\[\\[?\\|]]?\\)"
;;    :regexp "^\\(`[a-zA-Z0-9_-]+.*'\\)$"
;    :doc-spec '(
;                ("(python-lib-ja)Module Index" nil "^\\(`[a-zA-Z0-9_-]+.*'\\)$")
;                ("(python-lib-ja)Function-Method-Variable Index" nil "^\\(`[a-zA-Z0-9_-]+.*'\\)$")
;                ("(python-lib-ja)Class-Exception-Object Index" nil "^\\(`[a-zA-Z0-9_-]+.*'\\)$")
;                ("(python-lib-ja)Miscellaneous Index" nil "^\\(`[a-zA-Z0-9_-]+.*'\\)$")
;                )))

(provide 'init_python)
;;; init_python.el ends here
