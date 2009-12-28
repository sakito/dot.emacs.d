;;; init_ecb.el --- ecb

;; Copyright (C) 2008  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

;; cedet-1.0pre6で動作確認中
;; Load CEDET
(load-file "~/.emacs.d/lisp/cedet-1.0pre6/common/cedet.el")
(setq semantic-load-turn-useful-things-on t)
(setq semanticdb-default-save-directory "~/.emacs.d/var/semantic")

;; ECB : Emacs Code Browser
(require 'ecb)
;; バージョンチェックを実施しない
(setq ecb-version-check 'nil)
(setq ecb-auto-compatibility-check nil)
;; 起動時の tips 表示を停止
(setq ecb-tip-of-the-day nil)
;; レイアウト設定
;; デフォルトのレイアウトは "left8"
(setq ecb-layout-name "left3")

;; window幅
(setq ecb-windows-width 0.15)

(setq ecb-other-window-behavior 'edit-and-compile)
;; compile-window の高さ設定
(setq ecb-compile-window-height 4)
;; compile-window の横幅設定
(setq ecb-compile-window-width 'frame)
;; mode-line にwindow番号を表示しない
(setq ecb-mode-line-display-window-number nil)

;; eshell
(setq ecb-eshell-auto-activate t)


(setq ecb-options-version "2.40")
;(setq ecb-source-path (quote ("~/dev/" "~/work/")))

(provide 'init_ecb)
;;; init_ecb.el ends here
