;;; init_hiwin.el ---hiwin-mode

;; Copyright (C) 2011  sakito

;; Author: sakito  <sakito@sakito.com>
;; Keywords: tools

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

;; http://ksugita.blog62.fc2.com/blog-entry-8.html
;; http://sourceforge.jp/projects/gnupack/downloads/48407/hiwin-2.00.tar.gz/

;;; Code:
(require 'hiwin)
(hiwin-activate)

;; 色は暫定
(set-face-background 'hiwin-face "#586e75")

(provide 'init_hiwin)
;;; init_hiwin.el ends here
