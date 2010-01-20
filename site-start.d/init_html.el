;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_html.el --- html

;; Copyright (C) 2008  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:
;;; html-helper-mode
;; @see http://www.gest.unipd.it/~saint/hth.html
;; @see http://www.gest.unipd.it/~saint/hhm-documentation.htm
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(autoload 'jsp-html-helper-mode "html-helper-mode" "Yay HTML" t)

(setq html-helper-htmldtd-version "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
    \"http://www.w3.org/TR/html4/strict.dtd\">\n")
(setq html-helper-address-string 
  "<a href=\"mailto:sakito@sakito.com\">sakito&lt;sakito@sakito.com&gt;</a>")

(setq auto-mode-alist
      (append '(
                ("\\.jsp$" . jsp-html-helper-mode)
                )
              auto-mode-alist))

(provide 'init_html)
;;; init_html.el ends here
