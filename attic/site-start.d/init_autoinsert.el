;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_autoinsert.el --- auto-insert setting

;; Copyright (C) 2004-2012  sakito

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

;;  auto-insertの設定

;;; Code:

(require 'autoinsert)

;; ファイルを開いたら実行
(add-hook 'find-file-hooks 'auto-insert)

;; テンプレートとなるファイルがあるディレクトリ
;; 末尾に"/"が必要なので注意
(setq auto-insert-directory
      (expand-file-name "etc/autoinsert/" user-emacs-directory))

;; 質問しないで auto-insertを実行する
(setq auto-insert-query nil)

;; テンプレート用の置換文字列
(defvar template-replacements-alists
  '(
    ("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ("%module%"           . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
    ;;("%time%"             . (lambda () (format-time-string "%Y-%m-%d %k:%M:%S" (current-time))))
    ("%time%"             . (lambda () (format-time-string "%Y-%m-%d 00:00:00" (current-time))))
    ("%year%"             . (lambda () (format-time-string "%Y" (current-time))))
;    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
;    ("%include-guard%"    . (lambda () (format "__SCHEME_%s__" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))
    ))


;; 置換用の関数
(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (first c) (funcall (rest c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)

;; 各モードの設定

;; XML
(setq auto-insert-alist
      (append '((xml-mode . "xml-insert.xml"))
              auto-insert-alist))

;; Texinfo
(setq auto-insert-alist
      (append '((texinfo-mode . "texinfo.texi"))
              auto-insert-alist))


;; Perl
(add-to-list 'auto-insert-alist '(perl-mode . "perl.pl"))
(add-to-list 'auto-insert-alist '(cperl-mode . "perl.pl"))

;; Python
(setq auto-insert-alist
      (nconc '(
               ("\\.rst$" . ["rst.rst" my-template])
               (python-mode . ["python.py" my-template])
               ) auto-insert-alist))

;; Lisp
(setq auto-insert-alist
      (nconc '(
               ("\\.cl$" . ["cl.lisp" my-template])
               ("\\.lisp$" . ["cl.lisp" my-template])
               (lisp-mode . ["cl.lisp" my-template])
               ) auto-insert-alist))

;; Shell
(setq auto-insert-alist
      (nconc '(
               ("\\.sh$" . ["shell.sh" my-template])
               (sh-mode . ["shell.sh" my-template])
               ) auto-insert-alist))

;; Erlang
(setq auto-insert-alist
      (nconc '(
               ("\\.erl$" . ["erl.erl" my-template])
               (erlang-mode . ["erl.erl" my-template])
               ) auto-insert-alist))

;;(add-to-list 'auto-insert-alist '(emacs-lisp-mode  . "skelton.el"))
;;(add-to-list 'auto-insert-alist '(python-mode . "python.py"))


(provide 'init_autoinsert)
;;; init_autoinsert.el ends here
