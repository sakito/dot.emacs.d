;;; init_wgrep.el --- wgrep

;; Copyright (C) 2015 sakito

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

;; wgrep https://github.com/mhayashi1120/Emacs-wgrep
;; pt https://github.com/monochromegane/the_platinum_searcher
;; pt.el https://github.com/bling/pt.el

;;; Code:
(require 'pt)
(autoload 'wgrep-pt-setup "wgrep-pt")
(add-hook 'pt-search-mode-hook 'wgrep-pt-setup)
(setq wgrep-enable-key "r")

(provide 'init_wgrep)
;;; init_wgrep.el ends here
