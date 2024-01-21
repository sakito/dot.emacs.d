;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_color.el --- color setting file


;; Copyright (C) 2004-2012  sakito
;; Author: sakito <sakito@sakito.com>

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

;;; Commentary: 色の設定

;; 

;;; Code:

;; フォントロックの設定
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  ;;(setq font-lock-maximum-decoration t)
  (setq font-lock-support-mode 'jit-lock-mode))

;; タブ文字、全角空白、文末の空白の色付け
;; @see http://www.emacswiki.org/emacs/WhiteSpace
;; @see http://xahlee.org/emacs/whitespace-mode.html
(setq whitespace-style '(spaces tabs space-mark tab-mark))
(setq whitespace-display-mappings
      '(
       ;; (space-mark 32 [183] [46]) ; normal space, ·
        (space-mark 160 [164] [95])
        (space-mark 2208 [2212] [95])
        (space-mark 2336 [2340] [95])
        (space-mark 3616 [3620] [95])
        (space-mark 3872 [3876] [95])
        (space-mark ?\x3000 [?\□]) ;; 全角スペース
        ;; (newline-mark 10 [182 10]) ; newlne, ¶
        (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
        ))
(require 'whitespace)
;; (global-whitespace-mode 1) 常に whitespace-mode だと動作が遅くなる場合がある
(global-set-key (kbd "C-x w") 'global-whitespace-mode)

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)
;; EOB を表示
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; マーク領域を色付け
(setq transient-mark-mode t)

;; 変更点に色付け
(global-highlight-changes-mode t)
;; 初期は非表示として highlight-changes-visible-mode で表示する
(setq highlight-changes-visibility-initial-state nil)
(global-set-key (kbd "M-]") 'highlight-changes-next-change)
(global-set-key (kbd "M-[") 'highlight-changes-previous-change)

;; 現在行に色を付ける
;;(global-hl-line-mode)
;;(hl-line-mode 1)
;; 標準の hl-line だと結構邪魔なので拡張機能に変更
;; @see http://www.emacswiki.org/emacs/hl-line%2B.el
(require 'hl-line+)
(toggle-hl-line-when-idle 1)


;; 列に色を付ける
;; @see http://www.emacswiki.org/emacs/CrosshairHighlighting
;; @see http://www.emacswiki.org/emacs/VlineMode
;; @see http://www.emacswiki.org/cgi-bin/wiki/vline.el
;;(require 'crosshairs)

;; color-theme
(setq color-theme-load-all-themes nil)
(setq color-theme-libraries nil)
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (cond
      (mac-p
       (require 'color-theme-dark)
       (color-theme-dark))
      ;; (windows-p
      ;;  (require 'color-theme-ntemacs)
      ;;  (color-theme-ntemacs))
      (t
       (require 'color-theme-dark)
       (color-theme-dark))
      )))

;; face を調査するための関数
;; いろいろ知りたい場合は C-u C-x =
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

;; kill-ring 中の属性を削除
;; @see http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/junk_elisp.html
;; (defadvice kill-new (around my-kill-ring-disable-text-property activate)
;;   (let ((new (ad-get-arg 0)))
;;     (set-text-properties 0 (length new) nil new)
;;     ad-do-it))


(provide 'init_color)
;;; init_color.el ends here
