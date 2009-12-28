;;; init_ns.el --- ns

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

;; 

;;; Code:
;; 文字コード
(set-language-environment 'Japanese)
;; 極力UTF-8とする
(prefer-coding-system 'utf-8)

;(set-default-font
; "-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
;"-*-M+2P+IPAG circle-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1"
;)

;(setq mac-pass-control-to-system nil)
;(setq mac-pass-command-to-system nil)
;(setq mac-pass-option-to-system nil)

 (set-frame-parameter (selected-frame) 'alpha '(85 50))

(setq backup-by-copying t)
(setq backup-directory-alist
      '(
        ("^/etc/" . "~/.emacs.d/var/etc")
        ("." . "~/.emacs.d/var/emacs")
        ))

(when (< emacs-major-version 23)
 (setq fixed-width-use-QuickDraw-for-ascii t)
 (setq mac-allow-anti-aliasing t)
 (set-face-attribute 'default nil
                     :family "monaco"
                     :height 140)
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0208
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0212
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 ;;; Unicode フォント
 (set-fontset-font
  (frame-parameter nil 'font)
  'mule-unicode-0100-24ff
  '("monaco" . "iso10646-1"))
;;; キリル，ギリシア文字設定
;;; 注意： この設定だけでは古代ギリシア文字、コプト文字は表示できない
;;; http://socrates.berkeley.edu/~pinax/greekkeys/NAUdownload.html が必要
;;; キリル文字
 (set-fontset-font
  (frame-parameter nil 'font)
  'cyrillic-iso8859-5
  '("monaco" . "iso10646-1"))
;;; ギリシア文字
 (set-fontset-font
  (frame-parameter nil 'font)
  'greek-iso8859-7
  '("monaco" . "iso10646-1"))
 (setq face-font-rescale-alist
       '(("^-apple-hiragino.*" . 1.2)
         (".*osaka-bold.*" . 1.2)
         (".*osaka-medium.*" . 1.2)
         (".*courier-bold-.*-mac-roman" . 1.0)
         (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
         (".*monaco-bold-.*-mac-roman" . 0.9)
         ("-cdac$" . 1.3))))

(when (<= emacs-major-version 23)
(create-fontset-from-ascii-font
;"-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
"-*-Osaka-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
;"-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
;"-*-Lucida Grande-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1"
;"-*-M+2P+IPAG circle-normal-normal-normal-*-14-*-*-*-p-0-iso10646-1"
 nil "myhiraginomin")
(set-fontset-font "fontset-myhiraginomin" 'japanese-jisx0208
                  '("hiragino mincho pro" . "iso10646-*"))
(set-fontset-font "fontset-myhiraginomin" 'katakana-jisx0201
                  '("hiragino mincho pro" . "iso10646-*"))
(set-fontset-font "fontset-myhiraginomin" 'japanese-jisx0212
                  '("hiragino mincho pro" . "iso10646-*"))
(set-fontset-font "fontset-myhiraginomin" 'mule-unicode-0100-24ff
                  '("Lucida Grande" . "iso10646-*"))
(set-fontset-font "fontset-myhiraginomin" 'thai-tis620
                  '("ayuthaya" . "iso10646-*"))
(set-fontset-font "fontset-myhiraginomin" 'chinese-gb2312
                  '("stkaiti*" . "iso10646-*"))
(set-fontset-font "fontset-myhiraginomin" 'chinese-big5-1
                  '("lisong pro*" . "iso10646-*"))
(set-fontset-font "fontset-myhiraginomin" 'korean-ksc5601
                  '("applemyungjo*" . "iso10646-*"))

;; osaka = osaka + monaco
;; hiramaru = ヒラギノ丸ゴ + monaco
;; hirakaku_w3 = ヒラギノ角ゴ w3 + monaco
;; hirakaku_w6 = ヒラギノ角ゴ w6 + monaco
;; hirakaku_w8 = ヒラギノ角ゴ w8 + monaco
;; hiramin_w3 = ヒラギノ明朝 w3 + courier
;; hiramin_w6 = ヒラギノ明朝 w6 + courier

;; hiramaru = ヒラギノ丸ゴ + monaco
(create-fontset-from-ascii-font
"-*-Monaco-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
 nil "hiramaru")
(set-fontset-font "fontset-hiramaru" 'japanese-jisx0208
                  '("Hiragino Maru Gothic Pro" . "unicode-bmp"))
(set-fontset-font "fontset-hiramaru" 'katakana-jisx0201
                  '("Hiragino Kaku Gothic Pro" . "unicode-bmp"))
(set-fontset-font "fontset-hiramaru" 'japanese-jisx0212
                  '("Hiragino Kaku Gothic Pro" . "unicode-bmp"))
(set-fontset-font "fontset-hiramaru" 'thai-tis620
                  '("ayuthaya" . "unicode-bmp"))
(set-fontset-font "fontset-hiramaru" 'chinese-gb2312
                  '("stkaiti*" . "unicode-bmp"))
(set-fontset-font "fontset-hiramaru" 'chinese-big5-1
                  '("lisong pro*" . "unicode-bmp"))
(set-fontset-font "fontset-hiramaru" 'korean-ksc5601
                  '("applemyungjo*" . "unicode-bmp"))

(create-fontset-from-fontset-spec
 (concat
  "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-hiramaru2"
  ",ascii:-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
  ",japanese-jisx0208:hiragino maru gothic pro"
  ",katakana-jisx0201:hiragino maru gothic pro"
  ",japanese-jisx0212:hiragino maru gothic pro"
  ",unicode:-*-Osaka-*"
  ",thai-tis620:ayuthaya"
  ",chinese-gb2312:stkaiti*"
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


;; hirakaku_w6 = ヒラギノ角ゴ w6 + monaco(等幅にならない？)
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



(setq default-frame-alist
      (append (list 
               ;'(foreground-color . "snow")
               ;'(background-color . "black")
               ;'(border-color . "black")
               ;'(cursor-color . "orange")
               ;'(mouse-color . "orange")
;                    '(width . 100)
;                    '(height . 38)
;                    '(top . 250)
;                    '(left . 800)
;                    '(vertical-scroll-bars . nil)
                    '(font . "fontset-hiramaru")
;                    '(font . "fontset-osaka")
            ;;'(font . "fontset-mac")
            ;;'(font . "fontset-sakito")
                    )
              default-frame-alist)
      )
)

(provide 'init_ns)
;;; init_ns.el ends here
