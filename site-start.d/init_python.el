;;; init_python.el --- python.el settings

;; Copyright (C) 2012  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: languages, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; @see https://github.com/fgallina/python.el

;;; Code:
(require 'python)

;; env
(setenv "PYTHONSTARTUP"
        (expand-file-name "rc.d/pythonrc.py" user-emacs-directory))
(setenv "PYTHONPATH"
        (expand-file-name "~/local/lib/python2.7/site-packages"))

;; mode
(add-to-list 'auto-mode-alist '("\\.cgi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))
(add-to-list 'auto-mode-alist '("wscript" . python-mode))

(setq
  python-shell-interpreter "ipython"
  python-shell-interpreter-args "-i --pylab --colors=Linux"
  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; cython
(require 'cython-mode)

;; pep8
;; pip install pep8
;; python-pep8.el https://gist.github.com/302847
(require 'python-pep8)

;; pylint
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
    (add-to-list 'flymake-allowed-file-name-masks '("wscript$" flymake-python-init))
    (add-hook 'python-mode-hook (lambda () (flymake-mode t))))

;; Pymacs
(require 'pymacs)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

;; 補完
;; @see http://tic-tacs.blogspot.com/2012/01/emacsauto-complete-pycompletepython.html
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path
                (expand-file-name "lisp/python-mode" user-emacs-directory)))
(add-hook 'python-mode-hook '(lambda () (require 'pycomplete)))

;; jedi
(require 'jedi)

;; auto-complete
;(defun ac-python-mode-setup ()
;  (setq ac-sources (append ;;'(ac-source-python)
;                    ac-sources)))
;(add-hook 'python-mode-hook 'ac-python-mode-setup)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; 独自関数
(defun skt:python-import-modules-from-buffer ()
  (interactive)
  (with-current-buffer (current-buffer)
    (goto-char (point-min))
    (catch 'break
      (while (not (eobp))
        (catch 'continue
          (if (re-search-forward "^import .*" (point-max) t)
              (progn
                (sit-for 0.5)
                (python-shell-send-region (point-at-bol) (point-at-eol))
                (throw 'continue nil))
            (throw 'break nil))))))
  (message "All imports from `%s' done" (buffer-name)))

(defun skt:python-shell-send-file ()
  (interactive)
  (python-shell-send-file (buffer-file-name)))

;; hook
(defun skt:python-mode-hook ()
  (progn
    ;; キー
    (local-set-key (kbd "C-c ;") 'comment-dwim)
    (local-set-key (kbd "C-c :") 'comment-dwim)
    (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
    (local-set-key (kbd "C-c p") 'flymake-goto-prev-error)
    (local-set-key (kbd "C-c !") 'run-python)
    (local-set-key (kbd "C-c C-i") 'skt:python-import-modules-from-buffer)
    (local-set-key (kbd "C-c C-c") 'skt:python-shell-send-file)
    ))
(add-hook 'python-mode-hook 'skt:python-mode-hook)

(provide 'init_python)
;;; init_python.el ends here
