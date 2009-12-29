;;; init_objc.el --- objc

;; Copyright (C) 2009  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: languages

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

;; XCode 側設定
;; 環境設定->ファイルタイプ-> text -> sourcecode -> sourcecode.c -> その他 -> Emacs.app
;; 「その他」から選択すること。デフォルトに存在する emacs は Terminal.app が起動してしまう
;; 通常は以下を設定しないとフレームが新規作成される。パッチを当てていると不要
;; (setq ns-pop-up-frames nil)

;; 拡張子が m もしくは mm のファイルは matlab-mode とぶつかる
;; 拡張子が h のファイルをそのまま設定してしまうと C や C++ 開発で困る
;; 以下の設定は実質できない
;(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
;(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
;(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
;; magic-mode-alist を利用してファイル内容を解析してモード設定する
(setq magic-mode-alist
      (append (list
               '("\\(.\\|\n\\)*\n@implementation" . objc-mode)
               '("\\(.\\|\n\\)*\n@interface" . objc-mode)
               '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
              magic-mode-alist))

;; ヘッダファイルを開くには ヘッダファイルにカーソル併せて C-x C-f すれば良い
;; 上手く動作しないなら (ffap-bindings) を init.el に記述する。普通はデフォルトで on

(provide 'init_objc)
;;; init_objc.el ends here
