;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_color.el --- color setting file


;; Copyright (C) 2004  sakito
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

;; デフォルトのフレーム設定
(setq default-frame-alist
      (append (list
                    '(width . 140)
                    '(height . 55)
                    '(top . 90)
                    '(left . 500)
                    '(vertical-scroll-bars . nil)
                    )
              default-frame-alist)
      )

;; 背景の透過
;(set-frame-parameter (selected-frame) 'alpha '(95 15))
(add-to-list 'default-frame-alist '(alpha . (85 20)))

;;; フォントの設定
;; hiramaru = ヒラギノ丸ゴ + Menlo
(create-fontset-from-ascii-font
;; "-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 "-*-Menlo-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
;; "-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "hiramaru")
(set-fontset-font "fontset-hiramaru"
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Maru Gothic Pro" :registry "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'katakana-jisx0201
                  (font-spec :family "Hiragino Maru Gothic Pro" :registry "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'japanese-jisx0212
                  (font-spec :family "Hiragino Maru Gothic Pro" :registry "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'thai-tis620
                  (font-spec :family "Ayuthaya" :registry "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'chinese-gb2312
                  (font-spec :family "STHeiti" :registry "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'chinese-big5-1
                  (font-spec :family "LiSong Pro" :registry "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'korean-ksc5601
                  (font-spec :family "AppleGothic" :registry "iso10646-*"))
(add-to-list 'default-frame-alist '(font . "fontset-hiramaru"))
;; 等幅にするためのフォントサイズのリスケール
(setq face-font-rescale-alist '((".*Hiragino*" . 1.2)))


;; フォントロックの設定
;; hilit19はemacs19用で、メンテナンスされてません。
;; emacs2xではfont-lockを使うようにします。
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t)
  ;;(setq font-lock-maximum-decoration t)
  (setq font-lock-support-mode 'jit-lock-mode))

;; タブ文字、全角空白、文末の空白の色付け
;; @see http://www.emacswiki.org/emacs/WhiteSpace
;; @see http://xahlee.org/emacs/whitespace-mode.html
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

;; タブ文字、全角空白、文末の空白の色付け
;; font-lockに対応したモードでしか動作しません
(defface my-mark-tabs
  '(
    (t
     (:foreground "red" :underline t)
     )) nil)
(defface my-mark-whitespace
  '(
    (t
     (:background "gray")
     )) nil)
(defface my-mark-lineendspaces
  '(
    (t
     (:foreground "SteelBlue" :underline t)
     )) nil)

(defvar my-mark-tabs 'my-mark-tabs)
(defvar my-mark-whitespace 'my-mark-whitespace)
(defvar my-mark-lineendspaces 'my-mark-lineendspaces)

(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(
     ("\t" 0 my-mark-tabs append)
     ("　" 0 my-mark-whitespace append)
     ("[ \t]+$" 0 my-mark-lineendspaces append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; 行末の空白を表示
(setq-default show-trailing-whitespace t)
;; EOB を表示
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; マーク領域を色付け
(setq transient-mark-mode t)

;; 変更点に色付け
(global-highlight-changes-mode t)
(setq highlight-changes-visibility-initial-state t)
(global-set-key (kbd "M-]") 'highlight-changes-next-change)
(global-set-key (kbd "M-[")  'highlight-changes-previous-change)

;; 現在行に色を付ける
(global-hl-line-mode)
(hl-line-mode 1)

;; 列に色を付ける
;; @see http://www.emacswiki.org/emacs/CrosshairHighlighting
;; @see http://www.emacswiki.org/emacs/VlineMode
;; @see http://www.emacswiki.org/cgi-bin/wiki/vline.el
;;(require 'crosshairs)

;; color-thema
(setq color-theme-load-all-themes nil)
(setq color-theme-libraries nil)
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (require 'color-theme-sakito)
     (color-theme-sakito)))

;; face を調査するための関数
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
