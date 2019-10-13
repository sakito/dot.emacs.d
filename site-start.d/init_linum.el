;;; init_linum.el --- linum

;; Copyright (C) 2019 sakito

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
;; 行番号関連

;;; Code:
(toggle-scroll-bar nil)

(require 'linum)
(global-linum-mode t)

;; 特定モードのみで有効にする場合はhookで設定
;; (dolist (hook (list
;;                'c-mode-hook
;;                'emacs-lisp-mode-hook
;;                ))
;;   (add-hook hook '(lambda ()
;;                     (linum-mode 1))))

;; 表示フォーマット
(setq linum-format "%04d")

(provide 'init_linum)
;;; init_linum.el ends here
