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

;; フォントの設定
(add-to-list 'load-path (expand-file-name "~/Sites/develop/fixed-width-fontset/"))

;;; Code:
(provide 'init_fontset)

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

;; osaka等幅、monaco
;(create-fontset-from-fontset-spec
; (concat
;  "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-monaco16,"
;  "japanese-jisx0208:-apple-osaka-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
;  "katakana-jisx0201:apple-helvetica-medium-r-normal--14-140-75-75-m-140-mac-roman,"
;  "ascii:-apple-monaco-medium-r-normal-*-14-*-*-*-*-*-mac-roman"))
;(set-default-font "fontset-monaco16")
;(setq default-frame-alist (append '((font . "fontset-monaco16"))))


;; ヒラギノ,monaco
;(if (eq window-system 'mac)
;    (progn
;      (create-fontset-from-fontset-spec
;       (concat
;        "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-hiragino16,"
;        "japanese-jisx0208:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
;        "katakana-jisx0201:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--14-140-75-75-m-140-mac-roman,"
;        "ascii:-apple-monaco-medium-r-normal-*-14-*-*-*-*-*-mac-roman"))
;      (set-default-font "fontset-hiragino16")
;      (setq default-frame-alist (append '((font . "fontset-hiragino16"))))
;      ))

;(require 'carbon-font)

;(set-default-font "fontset-hiraginokaku")

;(setq default-frame-alist (append (list
;                                   '(font . "fontset-hiragino16")
;                                   )default-frame-alist))
;(setq face-font-rescale-alist (append (list
;                                       '(".*-bold.*" . 0.95)
;                                       )face-font-rescale-alist))

;(if (eq window-system 'mac)
;    (progn
;      (create-fontset-from-fontset-spec
;       (concat
;        "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-hiragino16,"
;        "japanese-jisx0208:-apple-ヒラギノ丸ゴ pro w4-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
;        "katakana-jisx0201:-apple-ヒラギノ丸ゴ pro w4-medium-r-normal--14-140-75-75-m-140-mac-roman,"
;        "ascii:-apple-monaco-medium-r-normal-*-14-*-*-*-*-*-mac-roman"))
;      (set-default-font "fontset-hiragino16")
;      (setq default-frame-alist (append '((font . "fontset-hiragino16"))))
;      ))

;(if (eq window-system 'mac)
;    (progn
;      (create-fontset-from-fontset-spec
;       (concat
;        "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-hiragino16,"
;        "japanese-jisx0208:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
;        "katakana-jisx0201:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--14-140-75-75-m-140-mac-roman,"
;        "latin-iso8859-1:-*-fixed-medium-i-normal-*-16-*-*-*-*-*-iso8859-1,"
;        "latin-iso8859-2:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-2,"
;        "latin-iso8859-3:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-3,"
;        "latin-iso8859-4:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-4,"
;        "cyrillic-iso8859-5:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-5,"
;        "greek-iso8859-7:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-7,"
;        "latin-iso8859-9:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-9,"
;        "vietnamese-viscii-lower:-*-fixed-*-*-*-*-16-*-*-*-*-*-viscii1.1-1,"
;        "vietnamese-viscii-upper:-*-fixed-*-*-*-*-16-*-*-*-*-*-viscii1.1-1"))
;      (set-default-font "fontset-hiragino16")
;      (setq default-frame-alist (append '((font . "fontset-hiragino16"))))
;      ))

;(if (eq window-system 'mac)
;    (progn
;      (create-fontset-from-fontset-spec
;       (concat
;        "-*-gentium-medium-r-normal-*-16-*-*-*-*-*-fontset-hiragino16,"
;        "japanese-jisx0208:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
;        "katakana-jisx0201:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--14-140-75-75-m-140-mac-roman,"
        ;"latin-iso8859-1:-*-fixed-medium-i-normal-*-16-*-*-*-*-*-iso8859-1,"
        ;"latin-iso8859-2:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-2,"
        ;"latin-iso8859-3:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-3,"
        ;"latin-iso8859-4:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-4,"
;        "cyrillic-iso8859-5:-apple-lucida grande-medium-r-normal--14-14-75-75-m-140-mac-roman,"
;        "greek-iso8859-7:-apple-lucida grande-medium-r-normal--14-14-75-75-m-140-mac-roman,"
        ;"latin-iso8859-9:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-9,"
        ;"vietnamese-viscii-lower:-*-fixed-*-*-*-*-16-*-*-*-*-*-viscii1.1-1,"
        ;"vietnamese-viscii-upper:-*-fixed-*-*-*-*-16-*-*-*-*-*-viscii1.1-1"
;        "ascii:-*-fixed-medium-r-normal-*-14-*-*-*-*-*-iso8859-7"
;        ))
;      (set-default-font "fontset-hiragino16")
;      (setq default-frame-alist (append '((font . "fontset-hiragino16"))))
;      ))


;; 全角 2 ： 半角 1 の実験
;(create-fontset-from-fontset-spec
; (concat
;  "-*-fixed-medium-r-normal-*-24-*-*-*-*-*-fontset-osaka24,"
;  "japanese-jisx0208:-apple-osaka\201|\223\231\225\235-medium-r-normal--24-240-75-75-m-240-jisx0208.1983-sjis,"
;  "katakana-jisx0201:-apple-osaka\201|\223\231\225\235-medium-r-normal--24-240-75-75-m-240-jisx0208.1983-sjis,"
;  "ascii:-apple-osaka-medium-r-normal--24-240-75-75-m-240-jisx0208.1983-sjis"))
;(set-default-font "fontset-osaka24")
;(setq default-frame-alist (append '((font . "fontset-osaka24"))))

;;false
;(create-fontset-from-fontset-spec
; (concat
;  "-*-fixed-medium-r-normal-*-24-*-*-*-*-*-fontset-hiragino24,"
;  "japanese-jisx0208:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--24-220-75-75-m-180-jisx0208.1983-sjis,"
;  "katakana-jisx0201:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--24-220-75-75-m-180-mac-roman,"
;  "ascii:-apple-times-medium-r-normal--24-240-75-75-m-240-mac-roman"))
;(set-default-font "fontset-hiragino24")
;(setq default-frame-alist (append '((font . "fontset-hiragino24"))))


;(create-fontset-from-fontset-spec
; (concat
;  "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-monaco16,"
;  "japanese-jisx0208:-apple-osaka-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
;  "katakana-jisx0201:apple-helvetica-medium-r-normal--14-140-75-75-m-140-mac-roman,"
;  "ascii:-apple-monaco-medium-r-normal-*-14-*-*-*-*-*-mac-roman"))
;(set-default-font "fontset-monaco16")
;;;(when (/= window-system 'x)
;(setq default-frame-alist (append '((font . "fontset-monaco16"))))
;;;)

;; 解像度により上記が大きい場合は以下
;(create-fontset-from-fontset-spec
; (concat
;  "-*-fixed-medium-r-normal-*-12-*-*-*-*-*-fontset-monaco12,"
;  "japanese-jisx0208:-apple-osaka-medium-r-normal--14-140-*-m-140-jisx0208.1983-sjis,"
;  "ascii:-apple-monaco-medium-r-normal-*-12-*-*-*-*-*-mac-roman"))
;(set-default-font "fontset-monaco12")
;(setq default-frame-alist (append '((font . "fontset-monaco12"))))



;; 以下はあくまでも参考です。エラーになる物があります。
;(create-fontset-from-fontset-spec
; (concat
;  "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-mikachan,"
;  "japanese-jisx0208:-misc-mikachan-medium-r-normal-*-14-*-*-*-*-*-jisx0208.1983-*,"
;  "katakana-jisx0201:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--14-140-75-75-m-140-mac-roman,"
;  "ascii:-apple-monaco-medium-r-normal-*-14-*-*-*-*-*-mac-roman"))
;(set-default-font "fontset-mikachan")
;(setq default-frame-alist (append '((font . "fontset-mikachan"))))

;; monaco
;(create-fontset-from-fontset-spec
; (concat
;  "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-monaco16,"
;  "japanese-jisx0208:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
;  "katakana-jisx0201:-apple-\203q\203\211\203m\203m\212\333\203s pro w4-medium-r-normal--14-140-75-75-m-140-mac-roman,"
;  "ascii:-apple-monaco-medium-r-normal-*-14-*-*-*-*-*-mac-roman"))
;(set-default-font "fontset-monaco16")
;(setq default-frame-alist (append '((font . "fontset-monaco16"))))

;(create-fontset-from-fontset-spec
; (concat
;  "-*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-monaco16,"
;  "japanese-jisx0208:-apple-osaka-medium-r-normal--16-160-75-75-m-160-jisx0208.1983-sjis,"
;  "ascii:-apple-monaco-medium-r-normal-*-14-*-*-*-*-*-mac-roman"))
;(set-default-font "fontset-monaco16")
;(when (/= window-system 'x)
;(setq default-frame-alist (append '((font . "fontset-monaco16"))))
;)

;"japanese-jisx0201:,"  
;(require 'bitmap)
;(set-face-font 'default "-*-fixed-medium-r-normal-*-18-*")
;(set-face-font 'default "-*-courier-medium-r-normal-*-18-*")
;(set-face-font 'default "-*-monaco-medium-r-normal-*-14-*")
;(if (fboundp 'new-fontset)
;    (progn
;      (create-fontset-from-fontset-spec
;       "-*-fixed-medium-r-normal-*-18-*-*-*-*-*-fontset-mac,
;        mac-roman-lower:-*-Monaco-*-*-*-*-14-*-*-*-*-*-mac-roman,
;        mac-roman-upper:-*-Monaco-*-*-*-*-14-*-*-*-*-*-mac-roman,
;        thai-tis620:-ETL-Fixed-*-*-*-*-16-*-*-*-*-*-tis620.2529-1,
;        lao:-Misc-Fixed-*-*-*-*-16-*-*-*-*-*-MuleLao-1,
;        vietnamese-viscii-lower:-ETL-Fixed-*-*-*-*-16-*-*-*-*-*-viscii1.1-1,
;        vietnamese-viscii-upper:-ETL-Fixed-*-*-*-*-16-*-*-*-*-*-viscii1.1-1,
;        chinese-big5-1:-*-Nice Taipei Mono-*-*-*-*-12-*-*-*-*-*-big5,
;        chinese-big5-2:-*-Nice Taipei Mono-*-*-*-*-12-*-*-*-*-*-big5,
;        chinese-gb2312:-*-Beijing-*-*-*-*-16-*-*-*-*-*-gb2312,
;        japanese-jisx0208:-apple-osaka-medium-r-normal--18-180-75-75-m-180-jisx0208.1983-sjis,
;        katakana-jisx0201:-*-*-*-*-*-*-16-*-*-*-*-*-JISX0201.1976-0,
;        korean-ksc5601:-*-Seoul-*-*-*-*-16-*-*-*-*-*-ksc5601"
;       t)))
;(load "~/src/intlfonts-1.2/bdf_intn")
;(load "~/src/intlfonts-1.2/bdf_intn24")


;;; init_fontset.el ends here
