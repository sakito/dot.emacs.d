;;; init_adoc.el --- AsciiDoc                        -*- lexical-binding: t; -*-

;; Copyright (C) 2024 sakitob

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools, abbrev, abbrev, abbrev

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; AsciiDoc
;; https://github.com/bbatsov/adoc-mode

;;; Code:
(unless (package-installed-p 'adoc-mode)
  (package-refresh-contents)
  (package-install 'adoc-mode))

;; txt を adocにする
(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))

(provide 'init_adoc)
;;; init_adoc.el ends here
