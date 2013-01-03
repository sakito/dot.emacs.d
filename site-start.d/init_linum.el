;;; init_linum.el --- linum

;; Copyright (C) 2011  sakito

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

;; https://github.com/myuhe/emacs-yalinum/blob/master/yalinum.el
;; http://sheephead.homelinux.org/2011/03/25/6706/

;;; Code:
(toggle-scroll-bar nil)

(require 'linum)
(global-linum-mode t)
;; フックにかける
;; (dolist (hook (list
;;                'c-mode-hook
;;                'emacs-lisp-mode-hook
;;                'lisp-interaction-mode-hook
;;                'lisp-mode-hook
;;                'java-mode-hook
;;                'js2-mode-hook
;;                'haskell-mode-hook
;;                'sh-mode-hook
;;                'python-mode-hook
;;                'ess-mode-hook
;;                'inferior-ess-mode-hook
;;                'lua-mode-hook
;;                'scala-mode-hook
;;                'haskell-mode-hook
;;                'css-mode-hook
;;                'makefile-mode-hook
;;                'rst-mode-hook
;;                ))
;;   (add-hook hook '(lambda ()
;;                     (linum-mode 1))))

;; 表示フォーマット
(setq linum-format "%04d")

;;yalinumの背景色の設定
;;(set-face-background 'yalinum-bar-face "DarkOliveGreen")

(provide 'init_linum)
;;; init_linum.el ends here
