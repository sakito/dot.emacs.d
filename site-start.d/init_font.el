;;; init_font.el --- emacs font                      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools, abbrev

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

;; フォント関連設定
;; frameサイズがフォントに影響されるため、frame関連の設定も同居

;;; Code:

;; 垂直スクロール用のスクロールバーを付けない
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))


;; デフォルトのフレーム設定
;; ディスプレイサイズによって分離する試み 途中
(cond
 ;; デュアルだったりトリプルだったりするので width の方は条件に入れてない
 ;; 設定は (frame-parameter (selected-frame) 'height) などで値を取得して設定する
 ((>= (display-pixel-height) 1440)
  (setq default-frame-alist
        (append (list
                 '(width . 172)
                 '(height . 60)
                 '(top . 123)
                 '(left . 420)
                 )
                default-frame-alist)))
 ;; 1920 * 1200 ディスプレイ
 ((= (display-pixel-height) 1200)
  (setq default-frame-alist
        (append (list
                 '(width . 175)
                 '(height . 65)
                 '(top . 50)
                 '(left . 500)
                 )
                default-frame-alist)))
 ;; MacBook Pro ディスプレイ
 ((= (display-pixel-height) 900)
  (setq default-frame-alist
        (append (list
                 '(width . 110)
                 '(height . 50)
                 '(top . 22)
                 '(left . 637)
                 )
                default-frame-alist)))
 ;; とりあえずその他 完全に未確認で分岐できる事を確認するためのコード
 (t
  (setq default-frame-alist
        (append (list
                 '(width . 140)
                 '(height . 50)
                 '(top . 90)
                 '(left . 100)
                 )
                default-frame-alist))))


;; 背景の透過
;; (add-to-list 'default-frame-alist '(alpha . (85 20)))
(add-to-list 'default-frame-alist '(alpha . (92 70)))

;;; フォントの設定
;; システム依存を排除するために一旦デフォルトフォントセットを上書き
;; 漢字は IPAゴジック + かな英数字は September を設定(等幅以外はインストールしてない)
;; jisx0208の範囲の漢字は September にすべきかもしれない
;; face の設定は基本的に全て color-thema に設定する方針
;; japanese-jisx0213.2004-1 = japanese-jisx0213-a + japanese-jisx0213-1
;; japanese-jisx0213-1 = japanese-jisx0208 のほぼ上位互換
;; japanese-jisx0213-2 = code-offset #x150000
;; japanese-jisx0212 = code-offset #x148000
;; japanese-jisx0208 = code-offset #x140000
(when mac-p
  (set-face-attribute 'default
                      nil
                      :family "Firge35"
                      :height 180)
  (set-frame-font "Firge35-18")
  (set-fontset-font nil
                    'unicode
                    (font-spec :family "Firge35")
                    nil
                    'append)
  ;; 古代ギリシア文字、コプト文字を表示したい場合は以下のフォントをインストールする
  ;; http://apagreekkeys.org/NAUdownload.html
  (set-fontset-font nil
                    'greek-iso8859-7
                    (font-spec :family "New Athena Unicode")
                    nil
                    'prepend)
  ;; 一部の文字を September にする
  ;; 記号         3000-303F http://www.triggertek.com/r/unicode/3000-303F
  ;; 全角ひらがな 3040-309f http://www.triggertek.com/r/unicode/3040-309F
  ;; 全角カタカナ 30a0-30ff http://www.triggertek.com/r/unicode/30A0-30FF
  (set-fontset-font nil
                    '( #x3000 .  #x30ff)
                    (font-spec :family "Firge35")
                    nil
                    'prepend)
  ;; 半角カタカナ、全角アルファベット ff00-ffef http://www.triggertek.com/r/unicode/FF00-FFEF
  (set-fontset-font nil
                    '( #xff00 .  #xffef)
                    (font-spec :family "Firge35")
                    nil
                    'prepend)

  ;; その他サンプル設定
  (when (find-font (font-spec :family "Menlo"))
    ;; ヒラギノ 角ゴ ProN + Menlo
    (create-fontset-from-ascii-font "Menlo-14" nil "menlokakugo")
    (set-fontset-font "fontset-menlokakugo"
                      'unicode
                      (font-spec :family "Hiragino Kaku Gothic ProN" :size 16))
    ;; 確認用 (set-frame-font "fontset-menlokakugo")
    ;; (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))  ;; 実際に設定する場合
    )
  )

;; linux では Ricty を利用している
(when linux-p
  (when (find-font (font-spec :family "Ricty"))
    ;; http://save.sys.t.u-tokyo.ac.jp/~yusa/fonts/ricty.html
    (set-face-attribute 'default
                        nil
                        :family "Ricty"
                        :height 140)
    (add-to-list 'default-frame-alist '(font . "Ricty-14"))
    (set-fontset-font nil
                      'unicode
                      (font-spec :family "Ricty")
                      nil
                      'append)
    ;; (set-frame-font "Ricty-16:weight=normal:slant=normal")
    ;; (set-frame-font "Aicty-14:weight=normal:slant=normal")
    ))


(provide 'init_font)
;;; init_font.el ends here
