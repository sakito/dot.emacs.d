;;; init_expand-region.el --- expand-region

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

;; https://github.com/magnars/expand-region.el
;; C-P 上移動
;; C-N 下移動
;; C-X 選択範囲先頭、末尾移動

;; https://github.com/magnars/mark-multiple.el

;;; Code:
(require 'expand-region)
;; リージョンを広げる
(global-set-key (kbd "C-@") 'er/expand-region)
;; リージョンを狭める
(global-set-key (kbd "C-M-@") 'er/contract-region)

(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)

(provide 'init_expand-region)
;;; init_expand-region.el ends here
