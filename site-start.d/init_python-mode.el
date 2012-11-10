;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_python-mode.el --- Python Mode Setting

;; Copyright (C) 2004-2010  sakito

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

;;; Commentary:

;; Python-mode および Pythonコーディングに便利な設定
;;; Python-mode
;; @see http://www.python.org/emacs/python-mode/
;; @see https://launchpad.net/python-mode/

;;; Code:

(setenv "PYTHONSTARTUP"
        (expand-file-name "rc.d/pythonrc.py" user-emacs-directory))
(setenv "PYTHONPATH"
        (expand-file-name "~/local/lib/python2.7/site-packages"))

(autoload 'python-mode "python-mode" "Python editing mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.cgi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; python
(cond
 (mac-p
  (setq py-python-command (expand-file-name "~/local/bin/python")))
 (windows-p
  (setq py-python-command "C:/Python27/bin/python"))
 )

;; pdb
(cond
 (mac-p
  (setq pdb-path '/Library/Frameworks/Python.framework/Versions/Current/lib/python2.7/pdb.py
        gud-pdb-command-name (symbol-name pdb-path)))
 )

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))

;; doctest
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(autoload 'doctest-mode
  "doctest-mode" "Editing mode for Python Doctest examples." t)

;; pip install pep8
;; python-pep8.el https://gist.github.com/302847
(require 'python-pep8)
;; pip install pylint-i18n
;; python-pylint.el https://gist.github.com/302848
(require 'python-pylint)
;; flymake
(when (load "flymake" t)
  (defun flymake-python-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "lintrunner.exe" (list local-file))))
    (add-to-list 'flymake-allowed-file-name-masks '("\\.py$" flymake-python-init))
    (add-hook 'python-mode-hook (lambda () (flymake-mode t))))

;; Pymacs
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; pip install rope
;; pip install ropemode
;; hg clone http://bitbucket.org/agr/ropemacs
;; easy_install -UZ ropemacs
;; Rope
;;(pymacs-load "ropemacs" "rope-")
;;(setq ropemacs-enable-autoimport t)

;; ipython
;; pip install ipython
;; pip install ipdb
(require 'ipython)
(setq ipython-command (expand-file-name "~/local/bin/ipython"))
(setq py-shell-switch-buffers-on-execute nil)
;; ipython の起動オプションを設定
;; デフォルトは (-i -colors LightBG)
;;(setq py-python-command-args '("-cl" "-i" "-colors" "Linux"))
;;(setq py-python-command-args '("-i" "-colors" "Linux"))
(setq-default py-python-command-args '("-i" "--pylab" "--colors=Linux"))
;;(setq py-python-command-args '("-cl" "-i" "Linux"))
;;(define-key py-mode-map (kbd "M-<tab>") 'anything-ipython-complete)
;;(define-key py-shell-map (kbd "M-<tab>") 'anything-ipython-complete)
;;(define-key py-mode-map (kbd "C-c M") 'anything-ipython-import-modules-from-buffer)

;; http://www.emacswiki.org/emacs/anything-ipython.el
(require 'anything-ipython)
(add-to-list 'anything-sources 'anything-source-ipython)
(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-ipython-complete
                                '(length initial-pattern)))

;; auto-complete
;; @see http://chrispoole.com/project/ac-python/
;;(require 'ac-python)

;; @see http://tic-tacs.blogspot.com/2012/01/emacsauto-complete-pycompletepython.html
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path
                (expand-file-name "lisp/python-mode" user-emacs-directory)))
(add-hook 'python-mode-hook '(lambda () (require 'pycomplete)))
;; auto-completeでpycompleteを渡すための設定
(defvar ac-source-pycomplete
  '((prefix "\\(?:\\.\\|->\\)\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (candidates . ac-pycomplete-candidates)
    (require . 0)))
(defun ac-pycomplete-candidates ()
  (pycomplete-get-all-completions (py-symbol-near-point) (py-find-global-imports)))
(defun ac-python-mode-setup ()
  (setq ac-sources (append '(ac-source-pycomplete)
                           ac-sources)))
(add-hook 'python-mode-hook 'ac-python-mode-setup)
(global-set-key (kbd "M-h") 'ac-complete-interactive-pycomplete)
(defvar ac-source-pycomplete-interactive
  '((prefix "\\(?:\\.\\|->\\)\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
    (candidates . ac-pycomplete-candidates)
    (requires . 0)))
(defun ac-complete-interactive-pycomplete ()
  (interactive)
  (auto-complete '(ac-source-pycomplete-interactive)))

;; function
(defun skt:py-execute-dwim ()
  "なんとなく状況に合せて実行 TODO"
  (interactive)
  (cond
   ;; class で初まる行の時
   ((or (py-statement-opens-class-p))
    (py-execute-class))
   ;; def で初まる行の時
   ((or (py-statement-opens-def-p))
    (py-execute-def))
   ;; return, raise 等で初まる行の時
   ((or (py-statement-closes-block-p))
    (py-execute-block))
   ;; block の場合
;   ((or (py-beginning-of-block-p)
;        (py-beginning-of-block))
;    (py-execute-block))
   ;; statement の場合
   ((or (py-beginning-of-statement-p)
        (py-beginning-of-statement))
    (py-execute-statement))
   ))

;; hook
(defun skt:python-mode-hook ()
  (progn
    ;; キー
    (local-set-key (kbd "C-c C-l") 'anything-ipython-complete) ;; py-shift-region-left を上書きしている
    ;;(local-set-key (kbd "C-c e") 'anything-ipython-complete)
    (local-set-key (kbd "C-c C-e") 'skt:py-execute-dwim)
    (local-set-key (kbd "C-c C-r") 'py-execute-region-no-switch)  ;; py-shift-region-right を上書きしている
    (local-set-key (kbd "C-c ;") 'comment-dwim)
    (local-set-key (kbd "C-c :") 'comment-dwim)
    ;; flymake
    (local-set-key (kbd "C-c C-w") 'flymake-mode-toggle)  ;; py-pychecker-run を上書きしている
    (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
    (local-set-key (kbd "C-c p") 'flymake-goto-prev-error)
    ))
(add-hook 'python-mode-hook 'skt:python-mode-hook)

(defun skt:ipython-shell-hook ()
  (progn
    ;; キー
    (local-set-key (kbd "C-c C-l") 'anything-ipython-complete)
    ))
(add-hook 'ipython-shell-hook 'skt:ipython-shell-hook)

;; anything で info 参照
;; もっと効率的に記述できることはわかっているが、都合により現在はこの記述にしておく

;; Info Python Module
(defvar anything-c-info-python-module nil)
(defvar anything-c-source-info-python-module
  `((name . "Info Python Module")
    (init . (lambda ()
              (save-window-excursion
                (unless anything-c-info-python-module
                  (with-temp-buffer
                    (Info-find-node "python-lib-jp" "Module Index")
                    (setq anything-c-info-python-module (split-string (buffer-string) "\n"))
                    (Info-exit))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-info-python-module
                          if (string-match "^* [^ \n]+[^: ]" i)
                          collect (match-string 0 i))))
    (action . (lambda (candidate)
                (Info-find-node "python-lib-jp" "Module Index")
                (Info-index (replace-regexp-in-string "* " "" candidate))))
    (requires-pattern . 2)))
;; (anything 'anything-c-source-info-python-module)

;; Info Python Class-Exception-Object
(defvar anything-c-info-python-class nil)
(defvar anything-c-source-info-python-class
  `((name . "Info Python Class-Exception-Object")
    (init . (lambda ()
              (save-window-excursion
                (unless anything-c-info-python-class
                  (with-temp-buffer
                    (Info-find-node "python-lib-jp" "Class-Exception-Object Index")
                    (setq anything-c-info-python-class (split-string (buffer-string) "\n"))
                    (Info-exit))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-info-python-class
                          if (string-match "^* [^ \n]+[^: ]" i)
                          collect (match-string 0 i))))
    (action . (lambda (candidate)
                (Info-find-node "python-lib-jp" "Class-Exception-Object Index")
                (Info-index (replace-regexp-in-string "* " "" candidate))))
    (requires-pattern . 2)))
;; (anything 'anything-c-source-info-python-class)

;; Info Python Function-Method-Variable
(defvar anything-c-info-python-function nil)
(defvar anything-c-source-info-python-function
  `((name . "Info Python Function-Method-Variable")
    (init . (lambda ()
              (save-window-excursion
                (unless anything-c-info-python-function
                  (with-temp-buffer
                    (Info-find-node "python-lib-jp" "Function-Method-Variable Index")
                    (setq anything-c-info-python-function (split-string (buffer-string) "\n"))
                    (Info-exit))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-info-python-function
                          if (string-match "^* [^ \n]+[^: ]" i)
                          collect (match-string 0 i))))
    (action . (lambda (candidate)
                (Info-find-node "python-lib-jp" "Function-Method-Variable Index")
                (Info-index (replace-regexp-in-string "* " "" candidate))))
    (requires-pattern . 2)))
;; (anything 'anything-c-source-info-python-function)

;; Info Python Miscellaneous
(defvar anything-c-info-python-misc nil)
(defvar anything-c-source-info-python-misc
  `((name . "Info Python Miscellaneous")
    (init . (lambda ()
              (save-window-excursion
                (unless anything-c-info-python-misc
                  (with-temp-buffer
                    (Info-find-node "python-lib-jp" "Miscellaneous Index")
                    (setq anything-c-info-python-misc (split-string (buffer-string) "\n"))
                    (Info-exit))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-info-python-misc
                          if (string-match "^* [^ \n]+[^: ]" i)
                          collect (match-string 0 i))))
    (action . (lambda (candidate)
                (Info-find-node "python-lib-jp" "Miscellaneous Index")
                (Info-index (replace-regexp-in-string "* " "" candidate))))
    (requires-pattern . 2)))
;; (anything 'anything-c-source-info-python-misc)



(provide 'init_python-mode)
;;; init_python-mode.el ends here
