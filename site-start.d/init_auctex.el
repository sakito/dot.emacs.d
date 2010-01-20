;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_auctex.el --- auctex setting file

;; Copyright (C) 2004  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools

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

;; 

;;; Code:

(setenv "TEXINPUT" "~/lib/texmf/tex/:~/lib/texinputs")
(require 'tex-site)
(require 'tex-jp)
(setq auto-mode-alist
      (append '(
                ("\\.tex$" . japanese-latex-mode)
                ("\\.ltx$" . japanese-latex-mode)
                )
              auto-mode-alist))
(modify-coding-system-alist 'file "\\.tex$" '(undecided . euc-jp))

(setq TeX-command-list
      (append (list
               (list "pLaTeXEuc" "platex-euc '\\nonstopmode\\input{%t}'"
                     'TeX-run-command nil t)
               (list "texi2html" "texi2html %t"
                     'TeX-run-command nil t)
               )
              TeX-command-list))

(add-hook 'TeX-mode-hook
          (function
           (lambda ()
             (setq TeX-command-default "pLaTeXEuc")
             (setq japanese-TeX-command-default "pTeXEuc")
             (setq japanese-LaTeX-command-default "pLaTeXEuc")
             )))

(add-hook 'TeXinfo-mode-hook
          (function
           (lambda ()
             (setq TeX-command-default "Makeinfo HTML")
              (define-key TeXinfo-mode-map "\C-c\C-e"
                'TeXinfo-insert-environment)
             )))

;; \documentclass{jsarticle} を標準にする
(setq japanese-LaTeX-default-style "jsarticle")
(setq kinsoku-limit 10)
(setq TeX-view-style
      '(
        ("." "~/opt/mxdvi/Mxdvi.app/Contents/MacOS/Mxdvi %d")
        ))

(provide 'init_auctex)
;;; init_auctex.el ends here
