;;; init_autoinsert.el --- auto-insert setting

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

;;; Commentary: auto-insertの設定

;; 

;;; Code:

(require 'autoinsert)
(add-hook 'find-file-hooks 'auto-insert)
;; テンプレートとなるファイルがあるディレクトリ
;; 末尾に"/"が必要なので注意
(setq auto-insert-directory "~/.emacs.d/etc/autoinsert/")
;; 質問しないで auto-insertを実行する
(setq auto-insert-query nil)

;; XMLへの追加
(setq auto-insert-alist
      (append '(
                (xml-mode . "xml-insert.xml")
                )
              auto-insert-alist))

;; Texinfoへの追加
(setq auto-insert-alist
      (append '(
                (texinfo-mode . "texinfo.texi")
                )
              auto-insert-alist))

;;(add-to-list 'auto-insert-alist '(emacs-lisp-mode  . "skelton.el"))
(add-to-list 'auto-insert-alist '(perl-mode . "perl.pl"))
(add-to-list 'auto-insert-alist '(cperl-mode . "perl.pl"))
(add-to-list 'auto-insert-alist '(python-mode . "python.py"))
;;(add-to-list 'auto-insert-alist '(ruby-mode . "skelton.rb"))
;;(add-to-list 'auto-insert-alist '("¥¥.tex¥¥'" . "tex-insert.tex"))
;;(add-to-list 'auto-insert-alist '("¥¥.html¥¥'" . "html4trans.html"))

(setq auto-insert-alist
      (nconc '(
               ("\\.rst$" . ["rst.rst" my-template])
               ) auto-insert-alist))


(defvar template-replacements-alists
  '(
    ("%file%"             . (lambda () (file-name-nondirectory (buffer-file-name))))
    ;;("%time%"             . (lambda () (format-time-string "%Y-%m-%d %k:%M:%S" (current-time))))
    ("%time%"             . (lambda () (format-time-string "%Y-%m-%d 00:00:00" (current-time))))
;    ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
;    ("%include-guard%"    . (lambda () (format "__SCHEME_%s__" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))
    ))

(defun my-template ()
  (time-stamp)
  (mapc #'(lambda(c)
            (progn
              (goto-char (point-min))
              (replace-string (car c) (funcall (cdr c)) nil)))
        template-replacements-alists)
  (goto-char (point-max))
  (message "done."))
(add-hook 'find-file-not-found-hooks 'auto-insert)


;; (defun my-insert-texinfo-template ()
;;   (let*
;;       (
;;        (title (read-string "title: "))
;;        (myname (concat user-full-name))
;;        (filename (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
;;        (current-date (current-time-string))
;;        )
;;     (progn
;;       (insert
;;        "\\input texinfo @c -*-texinfo-*-"
;;        "\n@c %**start of header"
;;        "\n@setfilename " filename ".info"
;;        "\n@settitle " title
;;        "\n@setchapternewpage odd"
;;        "\n@c %**end of header"
;;        "\n"
;;        "\n@c $Id$"
;;        "\n"
;;        "@today{}\n"
;;        "\n"
;;        "@titlepage\n"
;;        "@title @author @page\n"
;;        "@end titlepage\n"
;;        "@node Top, Front Matter, (dir), (dir)\n"
;;        "\n"
;;        "@top " title "\n"
;;        "\n"
;;        "@ifinfo\n"
;;        "@menu\n"
;;        "* Front Matter::\n"
;;        "@end menu\n"
;;        "\n"
;;        "@node Front Matter\n"
;;        "@unnumbered Front Matter\n"
;;        "\n"
;;        "@end ifinfo\n"
;;        "\n"
;;        "@contents\n"
;;        "@bye\n"
;;        )
;;       (beginning-of-buffer)
;;       )))

;; (setq auto-insert-alist
;;       (append
;;        '(
;;          (texinfo-mode . my-insert-texinfo-template)
;;          )
;;       auto-insert-alist))

(provide 'init_autoinsert)
;;; init_autoinsert.el ends here
