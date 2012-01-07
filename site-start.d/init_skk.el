;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_skk.el --- SKK setting file

;; Copyright (C) 2004  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; SKK の設定ファイル

;;; Code:

;; ddskkでは不要です
;; @see http://openlab.ring.gr.jp/skk/index-j.html
;;
;; 色設定
(setq skk-cursor-hiragana-color "hot pink")
(require 'skk-autoloads)
(setq skk-preload t)

;; C-\ でも SKK に切り替えられるように設定
(setq default-input-method "japanese-skk")

;(global-set-key "\C-x\C-j" 'skk-mode)
;(global-set-key "\C-xj" 'skk-auto-fill-mode)
;(global-set-key "\C-xt" 'skk-tutorial)
;(setq skk-large-jisyo "/usr/local/share/skk/SKK-JISYO.L")
;(setq skk-number-style 0)
;(setq date-ad 1)

(skk-mode)

(when (or mac-p linux-p)
  (setq skk-server-host "localhost")
  (setq skk-server-portnum 1178))
;(setq skk-jisyo-code 'utf-8-unix)

;;"「"を入力したら"」"も自動で挿入
;; (setq skk-auto-insert-paren t)

(setq skk-rom-kana-rule-list
      (append skk-rom-kana-rule-list
              '(("@" nil "@"))))

;; 送り仮名が厳密に正しい候補を優先して表示
(setq skk-henkan-strict-okuri-precedence t)

;;漢字登録時、送り仮名が厳密に正しいかをチェック
(setq skk-check-okurigana-on-touroku t)

;; 変換候補をインラインに表示
(setq skk-show-inline t)
;; 縦に表示したい場合以下を設定
(setq skk-show-inline 'vertical)

;; isearch時にSKKをオフ
(setq skk-isearch-start-mode 'latin)

;; カーソル付近に mode-string を表示(14.4 から)
;; popwin と同時利用したりすると動作が変な気がするので停止
;;(setq skk-show-mode-show t)
;;(setq skk-show-mode-style 'tooltip)

;; C-x C-fでファイルを開くとSKK
(add-hook 'find-file-hooks
          (lambda ()
            (skk-latin-mode t)
            ))

(add-hook 'skk-load-hook
          (lambda ()
            (require 'context-skk)
            (add-to-list 'context-skk-programming-mode 'scala-mode)
            ))

(defadvice skk-latin-mode (after no-latin-mode-in-lisp-interaction activate)
  "`lisp-interaction-mode' において英数モードを回避する。"
  (when (eq major-mode 'lisp-interaction-mode)
    (skk-mode-off)))

;; pos-tip が有効な時だけ以下の設定を実施
(when (require 'pos-tip nil t)
  ;; 変換時に注釈 (annotation) を表示する
  (setq skk-show-annotation t)
  ;; 変換候補一覧と注釈 (annotation) を tooltip で表示
  (setq skk-show-tooltip t)
  ;; Tip 描画に pos-tip を利用
  (setq skk-tooltip-function
        #'(lambda (tooltip-str)
            (pos-tip-show tooltip-str nil nil nil 0)))

  ;; Tooltip 表示位置の調整
  ;; (setq skk-tooltip-x-offset 3)
  ;; (setq skk-tooltip-y-offset 10)
  ;;(require 'popup)
  (when skk-show-tooltip
    ;; tooltip のルックスを指定する。デフォルトでは Emacs 標準のルックスになる
    (setq skk-tooltip-parameters
          '((background-color . "dim gray")
            (border-color . "dim gray"))))
  )

(provide 'init_skk)
;;; init_skk.el ends here
