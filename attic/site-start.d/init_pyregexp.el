;;; init_pyregexp.el --- pyregexp

;; Copyright (C) 2012  sakito

;; Author: sakito <sakito@sakito.com>
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

;; https://github.com/benma/pyregexp/

;;; Code:
(require 'pyregexp)
(define-key global-map (kbd "C-c r") 'pyregexp-replace)
(define-key global-map (kbd "C-c q") 'pyregexp-query-replace)
(define-key esc-map (kbd "C-r") 'pyregexp-isearch-backward)
(define-key esc-map (kbd "C-s") 'pyregexp-isearch-forward)

(provide 'init_pyregexp)
;;; init_pyregexp.el ends here
