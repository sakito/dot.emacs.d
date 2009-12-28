;;; init_yasnippet.el --- yasnippet

;; Copyright (C) 2009  sakito

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

;; @see http://code.google.com/p/yasnippet/

;; 

;;; Code:
(require 'yasnippet)

;; メニューは使わない
(setq yas/use-menu nil)

;; トリガはSPC, 次の候補への移動はTAB
;(setq yas/trigger-key (kbd "SPC"))
;(setq yas/next-field-key (kbd "TAB"))

;; http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el
(require 'anything-c-yasnippet)
(setq anything-c-yas-space-match-any-greedy t)
(global-set-key (kbd "C-c y") 'anything-c-yas-complete)

;; js2-mode へのyas/minor-mode の関連づけ 0.6.0以上では不要
;(add-to-list 'yas/extra-mode-hooks 'js2-mode-hook)

(yas/initialize)
(yas/load-directory "~/.emacs.d/etc/snippets")

(provide 'init_yasnippet)
;;; init_yasnippet.el ends here
