;;; early-init.el --- early-init                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  sakito

;; Author: sakito <sakito@sakito.com>

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

;; early-init

;;; Code:

;; fremeリサイズ抑止
(setq frame-inhibit-implied-resize t)

;; UI系不要物停止

;; Mac Emacsを利用している場合は以下の設定で停止しておく方が確実で、起動が早い
;; $ defaults write org.gnu.Emacs ToolBar -string no
;; $ defaults write org.gnu.Emacs MenuBar -string no
;; 確認 $ defaults read org.gnu.Emacs
;; 削除 $ defaults delete org.gnu.Emacs MenuBar

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; 有効化
(setq inhibit-redisplay t)
(setq inhibit-message t)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq inhibit-redisplay nil)
            (setq inhibit-message nil)
            (redisplay)))


;; 起動時のmessageを表示しない
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq byte-compile-warnings '(cl-functions))

;; 起動時の背景色
(custom-set-faces '(default ((t (:background "#073642")))))

(provide 'early-init)
;;; early-init.el ends here
