;;; init_po.el --- po file

;; Copyright (C) 2012  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools, languages

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

;; po file

;;; Code:
(autoload 'po-mode "po-mode" "Major mode for translators when they edit PO files." t)
(eval-after-load 'po-mode '(load "gb-po-mode"))

(provide 'init_po)
;;; init_po.el ends here