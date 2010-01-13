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


;;; フォントの設定
;(require 'init_fontset)
;; see http://www.emacswiki.org/emacs/SetFonts
;; see http://macemacsjp.sourceforge.jp/matsuan/FontSettingJp.html
;; see http://sourceforge.jp/projects/macemacsjp/lists/archive/users/2005-November/000780.html
; M-x mac-font-panel-mode
; M-x describe-font
;(set-default-font
;(setq fixed-width-use-QuickDraw-for-ascii t)
;(setq mac-allow-anti-aliasing t)
(create-fontset-from-ascii-font
;"-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
;"-*-Osaka-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
"-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
;"-*-Lucida Grande-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
;"-*-M+2P+IPAG circle-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "myhiramin")
(set-fontset-font "fontset-myhiramin" 'japanese-jisx0208
                  '("hiragino mincho pro" . "iso10646-*"))
(set-fontset-font "fontset-myhiramin" 'katakana-jisx0201
                  '("hiragino mincho pro" . "iso10646-*"))
(set-fontset-font "fontset-myhiramin" 'japanese-jisx0212
                  '("hiragino mincho pro" . "iso10646-*"))
;(set-fontset-font "fontset-myhiraginomin" 'mule-unicode-0100-24ff
;                  '("Lucida Grande" . "iso10646-*"))
(set-fontset-font "fontset-myhiramin" 'thai-tis620
                  '("ayuthaya" . "iso10646-*"))
(set-fontset-font "fontset-myhiramin" 'chinese-gb2312
                  '("stkaiti*" . "iso10646-*"))
(set-fontset-font "fontset-myhiramin" 'chinese-big5-1
                  '("lisong pro*" . "iso10646-*"))
(set-fontset-font "fontset-myhiramin" 'korean-ksc5601
                  '("applemyungjo*" . "iso10646-*"))

;; osaka = osaka + monaco
;; hiramaru = ヒラギノ丸ゴ + monaco
;; hirakaku_w3 = ヒラギノ角ゴ w3 + monaco
;; hirakaku_w6 = ヒラギノ角ゴ w6 + monaco
;; hirakaku_w8 = ヒラギノ角ゴ w8 + monaco
;; hiramin_w3 = ヒラギノ明朝 w3 + courier
;; hiramin_w6 = ヒラギノ明朝 w6 + courier

;; osaka等幅:  -*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1
;; monaco等幅: -*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1
;; courier等幅:-*-Courier-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1

;; M-x set-default-font
;; ヒラギノ角ゴ Pro W3:Hiragino Kaku Gothic Pro-normal-normal-normal
;; ヒラギノ角ゴ Pro W6:Hiragino Kaku Gothic Pro-bold-normal-normal
;; ヒラギノ角ゴ ProN W3:Hiragino Kaku Gothic ProN-normal-normal-normal
;; ヒラギノ角ゴ ProN W6:Hiragino Kaku Gothic ProN-bold-normal-normal
;; ヒラギノ角ゴ Std W8:Hiragino Kaku Gothic Std-wnormal-normal-normal
;; ヒラギノ角ゴ StdN W8:Hiragino Kaku Gothic StdN-normal-normal-normal
;; ヒラギノ丸ゴ Pro W4:Hiragino Maru Gothic Pro-normal-normal-normal
;; ヒラギノ丸ゴ ProN W4:Hiragino Maru Gothic ProN-normal-normal-normal
;; ヒラギノ明朝 Pro W3:Hiragino Mincho Pro-normal-normal-normal
;; ヒラギノ明朝 Pro W6:Hiragino Mincho Pro-bold-normal-normal
;; ヒラギノ明朝 ProN W3:Hiragino Mincho ProN-normal-normal-normal
;; ヒラギノ明朝 ProN W6:Hiragino Mincho ProN-bold-normal-normal

;(setq-default line-spacing 0.3)

;; hiramaru = ヒラギノ丸ゴ + monaco
(create-fontset-from-ascii-font
 "-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
; "-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "hiramaru")
(set-fontset-font "fontset-hiramaru" 'japanese-jisx0208
                  '("Hiragino Maru Gothic Pro" . "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'katakana-jisx0201
                  '("Hiragino Maru Gothic Pro" . "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'japanese-jisx0212
                  '("Hiragino Maru Gothic Pro" . "iso10646-*"))
;(set-fontset-font "fontset-hiramaru" 'mule-unicode-0100-24ff
;                  '("Monaco" . "iso10646-1"))
(set-fontset-font "fontset-hiramaru" 'thai-tis620
                  '("Ayuthaya" . "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'chinese-gb2312
                  '("STHeiti*" . "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'chinese-big5-1
                  '("LiSong Pro*" . "iso10646-*"))
(set-fontset-font "fontset-hiramaru" 'korean-ksc5601
                  '("AppleGothic*" . "iso10646-*"))

(create-fontset-from-fontset-spec
 (concat
  "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-hiramaru2"
  ",ascii:-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
  ",japanese-jisx0208:hiragino maru gothic pro"
  ",katakana-jisx0201:hiragino maru gothic pro"
  ",japanese-jisx0212:hiragino maru gothic pro"
  ",unicode:Osaka"
  ",thai-tis620:ayuthaya"
  ",chinese-gb2312:stheiti*"
  ",chinese-big5-1:lisong pro*"
  ",korean-ksc5601:applegothic*"
  ))

;; hirakaku_w3 = ヒラギノ角ゴ w3 + monaco
(create-fontset-from-ascii-font
"-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "hirakaku_w3")
(set-fontset-font "fontset-hirakaku_w3" 'japanese-jisx0208
                  '("Hiragino Kaku Gothic Pro W3" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w3" 'katakana-jisx0201
                  '("Hiragino Kaku Gothic Pro W3" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w3" 'japanese-jisx0212
                  '("Hiragino Kaku Gothic Pro W3" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w3" 'thai-tis620
                  '("ayuthaya" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w3" 'chinese-gb2312
                  '("stkaiti*" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w3" 'chinese-big5-1
                  '("lisong pro*" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w3" 'korean-ksc5601
                  '("applemyungjo*" . "unicode-bmp"))


;; hirakaku_w6 = ヒラギノ角ゴ w6 + monaco
(create-fontset-from-ascii-font
"-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "hirakaku_w6")
(set-fontset-font "fontset-hirakaku_w6" 'japanese-jisx0208
                  '("Hiragino Kaku Gothic Pro W6" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w6" 'katakana-jisx0201
                  '("Hiragino Kaku Gothic Pro W6" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w6" 'japanese-jisx0212
                  '("Hiragino Kaku Gothic Pro W6" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w6" 'thai-tis620
                  '("ayuthaya" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w6" 'chinese-gb2312
                  '("stkaiti*" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w6" 'chinese-big5-1
                  '("lisong pro*" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w6" 'korean-ksc5601
                  '("applemyungjo*" . "unicode-bmp"))


;; hirakaku_w8 = ヒラギノ角ゴ w8 + monaco
(create-fontset-from-ascii-font
"-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "hirakaku_w8")
(set-fontset-font "fontset-hirakaku_w8" 'japanese-jisx0208
                  '("Hiragino Kaku Gothic Pro W8" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w8" 'katakana-jisx0201
                  '("Hiragino Kaku Gothic Pro W8" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w8" 'japanese-jisx0212
                  '("Hiragino Kaku Gothic Pro W8" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w8" 'thai-tis620
                  '("ayuthaya" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w8" 'chinese-gb2312
                  '("stkaiti*" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w8" 'chinese-big5-1
                  '("lisong pro*" . "unicode-bmp"))
(set-fontset-font "fontset-hirakaku_w8" 'korean-ksc5601
                  '("applemyungjo*" . "unicode-bmp"))


;; hiramin_w3 = ヒラギノ明朝 w3 + courier
(create-fontset-from-ascii-font
"-*-Courier-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "hiramin_w3")
(set-fontset-font "fontset-hiramin_w3" 'japanese-jisx0208
                  '("Hiragino Mincho Pro W3" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w3" 'katakana-jisx0201
                  '("Hiragino Mincho Pro W3" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w3" 'japanese-jisx0212
                  '("Hiragino Mincho Pro W3" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w3" 'thai-tis620
                  '("ayuthaya" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w3" 'chinese-gb2312
                  '("stkaiti*" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w3" 'chinese-big5-1
                  '("lisong pro*" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w3" 'korean-ksc5601
                  '("applemyungjo*" . "unicode-bmp"))

;; hiramin_w6 = ヒラギノ明朝 w6 + courier
(create-fontset-from-ascii-font
"-*-Courier-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "hiramin_w6")
(set-fontset-font "fontset-hiramin_w6" 'japanese-jisx0208
                  '("Hiragino Mincho Pro W6" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w6" 'katakana-jisx0201
                  '("Hiragino Mincho Pro W6" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w6" 'japanese-jisx0212
                  '("Hiragino Mincho Pro W6" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w6" 'thai-tis620
                  '("ayuthaya" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w6" 'chinese-gb2312
                  '("stkaiti*" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w6" 'chinese-big5-1
                  '("lisong pro*" . "unicode-bmp"))
(set-fontset-font "fontset-hiramin_w6" 'korean-ksc5601
                  '("applemyungjo*" . "unicode-bmp"))

;(set-default-font "fontset-hiramaru")
;(set-default-font "fontset-hiramin")
;(set-frame-font "fontset-hiramaru")
;;色の設定です変更してください
;;コメントアウトしてるのはうまく効かないもの
(setq default-frame-alist
      (append (list 
               ;'(foreground-color . "snow")
               ;'(background-color . "black")
               ;'(border-color . "black")
               ;'(cursor-color . "orange")
               ;'(mouse-color . "orange")
                    '(width . 140)
                    '(height . 55)
;                    '(top . 250)
                    '(top . 90)
;                    '(left . 800)
                    '(left . 500)
                    '(vertical-scroll-bars . nil)
;                    '(font . "fontset-hiramaru")
;                    '(font . "fontset-hirakaku_w3")
            ;;'(font . "fontset-mac")
            ;;'(font . "fontset-sakito")
                    )
              default-frame-alist)
      )

;; 背景の透過
;(set-frame-parameter (selected-frame) 'alpha '(95 15))
(add-to-list 'default-frame-alist '(alpha . (85 20)))

;; フォントの設定
(add-to-list 'default-frame-alist '(font . "fontset-hiramaru"))

;; フォントロックの設定
;; hilit19はemacs19用で、メンテナンスされてません。
;; emacs2xではfont-lockを使うようにします。
(cond (
       (fboundp 'global-font-lock-mode)
       (global-font-lock-mode t)
       ;(setq font-lock-maximum-decoration t)
       (setq font-lock-support-mode 'jit-lock-mode)
       ))

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
;; 最後の行移行に indicat を表示
(setq-default indicate-empty-lines t)

;; マーク領域を色付け
(setq transient-mark-mode t)

;; 現在行に色を付ける
(global-hl-line-mode)
(hl-line-mode 1)

;; 列に色を付ける
;; @see http://www.emacswiki.org/emacs/CrosshairHighlighting
;; @see http://www.emacswiki.org/emacs/VlineMode
;; @see http://www.emacswiki.org/cgi-bin/wiki/vline.el
;;(require 'crosshairs)

;; color-thema
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-sakito)))

;; face を調査するための関数
(defun describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(provide 'init_color)
;;; init_color.el ends here
