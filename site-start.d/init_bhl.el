;;; init_bhl.el --- bhl mode setting file

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

;; BHL mode の設定

;;; Code:

(autoload 'bhl-mode "bhl" "BHL Mode" t)
(add-to-list 'auto-mode-alist '("\\.bhl$" . bhl-mode))
(setq bhl-sectioning-default-style 'equal-sign)
(setq bhl-i18n-conventions '("jp" t t t))
(setq bhl2html-properties-list '(t nil nil t t))
(setq bhl-html-title-tags '("<h1 class=\"title\">" "</h1>"))
(setq bhl-html-subtitle-tags '("<h2 class=\"subtitle\">" "</h2>"))
(setq bhl-html-doctype 
      "<?xml version=\"1.0\" encoding=\"euc-jp\"?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
(setq bhl-html-content-type
      "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=euc-jp\" />")
(setq bhl-html-default-style "<link rel=\"stylesheet\" type=\"text/css\" charset=\"iso-8859-1\" media=\"screen\" href=\"style.css\" />")


(provide 'init_bhl)
;;; init_bhl.el ends here
