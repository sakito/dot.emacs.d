;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_fontset.el --- fontset

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

;;; Commentary:

;; フォントの設定を集めたもの
;; 実験した物など様々雑多な物が混在しており、正常に動作しない物を普通にのこっています。

(add-to-list 'load-path (expand-file-name "~/Sites/develop/fixed-width-fontset/"))

;;; Code:
(provide 'init_fontset)

;(setq-default line-spacing 0.3)

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

;;  create-fontset-from-fontset-spec によるフォント設定

;; (create-fontset-from-fontset-spec
;;  (concat
;;   "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-hiramin"
;;   ",ascii:-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
;;   ",japanese-jisx0208:hiragino mincho pro"
;;   ",katakana-jisx0201:hiragino mincho pro"
;;   ",unicode:-apple-osaka-*"
;;   ",thai-tis620:ayuthaya"
;;   ",chinese-gb2312:stkaiti*"
;;   ",chinese-big5-1:lisong pro*"
;;   ",korean-ksc5601:applemyungjo*"
;;   ))

(create-fontset-from-fontset-spec
 (concat
  "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-osaka"
  ",ascii:-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
  ",japanese-jisx0208:-*-Osaka-*"
  ",katakana-jisx0201:-*-Osaka-*"
  ",unicode:-*-Osaka-*"
  ",chinese-gb2312:stkaiti*"
  ",chinese-big5-1:lisong pro*"
  ",korean-ksc5601:applegothic*"
  ))

(create-fontset-from-fontset-spec
 (concat
  "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-hiramaru"
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

(create-fontset-from-fontset-spec
 (concat
  "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-hirakaku_w3"
  ",ascii:-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
  ",japanese-jisx0208:Hiragino Kaku Gothic Pro"
  ",katakana-jisx0201:Hiragino Kaku Gothic Pro"
  ",japanese-jisx0212:Hiragino Kaku Gothic Pro"
  ",unicode:-*-Osaka-*"
  ",thai-tis620:ayuthaya"
  ",chinese-gb2312:stkaiti*"
  ",chinese-big5-1:lisong pro*"
  ",korean-ksc5601:applegothic*"
  ))

(create-fontset-from-fontset-spec
 (concat
  "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-hirakaku_w8"
  ",ascii:-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
  ",japanese-jisx0208:Hiragino Kaku Gothic Std"
  ",katakana-jisx0201:Hiragino Kaku Gothic Std"
  ",japanese-jisx0212:Hiragino Kaku Gothic Std"
  ",unicode:-*-Osaka-*"
  ",thai-tis620:ayuthaya"
  ",chinese-gb2312:stkaiti*"
  ",chinese-big5-1:lisong pro*"
  ",korean-ksc5601:applegothic*"
  ))

(create-fontset-from-fontset-spec
 (concat
  "-*-fixed-medium-r-normal--14-*-*-*-*-*-fontset-hiramin_w3"
  ",ascii:-*-Osaka-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
  ",japanese-jisx0208:Hiragino Mincho Pro"
  ",katakana-jisx0201:Hiragino Mincho Pro"
  ",japanese-jisx0212:Hiragino Mincho Pro"
  ",unicode:-*-Osaka-*"
  ",thai-tis620:ayuthaya"
  ",chinese-gb2312:stkaiti*"
  ",chinese-big5-1:lisong pro*"
  ",korean-ksc5601:applegothic*"
  ))



;(if (eq window-system 'mac)
(when (< emacs-major-version 23)
    (progn
;      (require 'init_fontset)
      (require 'carbon-font)
      (fixed-width-set-fontset "hiramaru" 14)
      ;(set-default-font "fontset-hiraginokaku")
      ))

;(setq my-font "-*-*-medium-r-normal--14-*-*-*-*-*-fontset-hiramaru")
;(set-default-font my-font)
;(add-to-list 'default-frame-alist `(font . ,my-font))
;(if (eq window-system 'ns)
;; @see http://d.hatena.ne.jp/kazu-yamamoto/20090122/1232589385
;; @see http://nox-insomniae.ddo.jp/insomnia/2007/11/emacsapp.html
(when (>= emacs-major-version 23)
  (setq fixed-width-use-QuickDraw-for-ascii t)
  (setq mac-allow-anti-aliasing t)
  (set-face-attribute 'default nil
                      :family "monaco"
                      :height 140)

  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0208
;   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
   '("ヒラギノ丸ゴ pro w4" . "iso10646-1"))

  (set-fontset-font
   (frame-parameter nil 'font)
   'katakana-jisx0201
;   '("Hiragino Maru Gothic Pro" . "iso10646-1"))
   '("ヒラギノ丸ゴ pro w4" . "iso10646-1"))

  (set-fontset-font
   (frame-parameter nil 'font)
   'japanese-jisx0212
   '("Hiragino Maru Gothic Pro" . "iso10646-1"))

  ;;; Unicode フォント
  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-0100-24ff
   '("monaco" . "iso10646-1"))

  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-2500-33ff
   '("monaco" . "iso10646-1"))

  (set-fontset-font
   (frame-parameter nil 'font)
   'mule-unicode-e000-ffff
   '("monaco" . "iso10646-1"))

  ;; Unicode フォント設定がしてあれば以下はすべて共通になるので設定しない
  ;; Latin-1 西欧 latin-iso8859-1
  ;; Latin-2 東欧 latin-iso8859-2
  ;; Latin-3 エスペラント latin-iso8859-3
  ;; Latin-4 北欧 latin-iso8859-4
  ;; キリル文字 cyrillic-iso8859-5
  ;; ギリシア文字 greek-iso8859-7
  ;; ヘブライ語 hebrew-iso8859-8
  ;; Latin-9 latin-iso8859-9

  ;; 古代ギリシア文字、コプト文字を表示したい場合は以下のフォントをインストールする
  ;; http://apagreekkeys.org/NAUdownload.html

  ;; タイ語
  (set-fontset-font
   (frame-parameter nil 'font)
   'thai-tis620
   '("ayuthaya" . "iso10646-1"))

  ;; 中国語(簡体字)
  (set-fontset-font
   (frame-parameter nil 'font)
   'chinese-gb2312
   '("stheiti*" . "iso10646-1"))

  ;; 中国語(繁体字)
  (set-fontset-font
   (frame-parameter nil 'font)
   'chinese-big5-1
   '("lihei pro*" . "iso10646-1"))

  (set-fontset-font
   (frame-parameter nil 'font)
   'chinese-big5-2
   '("lihei pro*" . "iso10646-1"))

  ;; 韓国語
  (set-fontset-font
   (frame-parameter nil 'font)
   'korean-ksc5601
   '("applegothic*" . "iso10646-1"))

  ;; 以下はMacの標準フォントに存在しないと思われるため別途フォントのインストールが必要
  ;; ethiopic アラハラ語、ゲエズ文字(Amharic) エチオピア公用語
  ;; 参考サイト http://www.wazu.jp/gallery/Fonts_Ethiopic.html
  ;; Bengali バングラデシュ公用語
  ;; 参考サイト http://www.wazu.jp/gallery/Fonts_Bengali.html
  ;; Kannada カンナダ語 インド南部カルナータカ州公用語
  ;; 参考サイト http://www.wazu.jp/gallery/Fonts_Kannada.html
  ;; Khmer クメール語 カンボジア、ベトナム
  ;; 参考サイト http://www.wazu.jp/gallery/Fonts_Khmer.html
  ;; Malayalam マラヤーラム語 インド南部ケーララ州公用語
  ;; 参考サイト http://www.wazu.jp/gallery/Fonts_Malayalam.html
  ;; Oriya オリヤー語 インド南部インドオリッサ州公用語
  ;; 参考サイト http://www.wazu.jp/gallery/Fonts_Oriya.html
  ;; Sinhala シンハラ語 スリランカ公用語の一つ
  ;; 参考サイト http://www.wazu.jp/gallery/Fonts_Sinhala.html
  ;; Telugu テルグ語 インド南東部アーンドラ・プラデーシュ州公用語
  ;; 参考サイト http://www.wazu.jp/gallery/Fonts_Telugu.html
  ;; Tigrigna
  ;; Myanmar(Burmese)、 ミャンマー語(ビルマ語)
  ;; http://www.wazu.jp/gallery/Fonts_Myanmar.html

  (setq face-font-rescale-alist
        '((".*monaco-bold-.*-mac-roman" . 0.9)
          (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
          (".*courier-bold-.*-mac-roman" . (( "9" . 0.9) ("10" . 0.9)))
          (".*osaka-medium.*" . 1.2)
          (".*osaka-bold.*" . 1.2)
          ("^-apple-hiragino.*" . 1.2)
          (,(encode-coding-string ".*ヒラギノ丸ゴ pro w4.*" 'emacs-mule) . 1.2)
          ("-cdac$" . 1.3))))

;;; init_fontset.el ends here
