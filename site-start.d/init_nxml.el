;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_nxml.el --- nxml-mode setting

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

;;; Commentary: nxml-modeを便利に利用するための設定

;; 

;;; Code:

;;; nxml-mode
;; @http://www.thaiopensource.com/download/
;(load "~/.emacs.d/lisp/nxml-mode-20041004/rng-auto.el")
(setq auto-mode-alist
      (cons '("\\.\\(xml\\|xsl\\|rng\\|sdoc\\|xhtml\\|html\\)\\'" . nxml-mode)
            auto-mode-alist))
;; スラッシュの入力で終了タグを自動補完
(setq nxml-slash-auto-complete-flag t)

(add-hook 'nxml-mode-hook
          (lambda()
            ;; キーの設定
            (define-key nxml-mode-map (kbd "\t") 'nxml-complete)
            (define-key nxml-mode-map (kbd "C-c C-v") 'browse-url-of-file)
            ))

;(custom-set-variables
; '(rng-schema-locating-files (quote ("schemas.xml" "~/.emacs.d/share/rnc/schemas.xml" "~/.emacs.d/lisp/nxml-mode-20031031/schema/schemas.xml")))
; )

;; rng-auto-file-name-alistの設定
;(setq rng-auto-file-name-alist
;      (append
;       (list
;        '(".*\\.xslt\\'" "~/.emacs.d/lisp/nxml-mode/schema/xslt.rnc")
;        )
;       rng-auto-file-name-alist-default)
;      )

(add-hook 'sgml-mode-hook
          (function
           (lambda ()
             (nxml-mode)
             )))

(provide 'init_nxml)
;;; init_nxml.el ends here
