;;; init_muse.el --- Emacs Muse setting

;; Copyright (C) 2008-2010  sakito

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
;; モードの読み込み
(require 'muse-mode)

; 利用する出力設定
(require 'muse-html)
(require 'muse-latex)
(require 'muse-texinfo)
(require 'muse-docbook)

(require 'muse-project)

;; 文字コード設定
(setq muse-html-encoding-default 'iso-2022-jp-unix)

;(add-to-list 'muse-project-alist
;             '("Default"
;               ("~/work" :default "index")
;               (:base "html" :path "~/work")))
;(add-to-list 'muse-project-alist
;             `("blogger"
 ;              ("pubilc-html-lisp" ("~/work"
;                                    :set (muse-html-header
;                                          "<html><body>Simple Header.\n")))
                ;;(:base "html" :path "~/work/lisp"))))
;
;                (:base "html" :path "~/work")))


;; bloggerの日記を快適に書くための設定
(require 'muse-blosxom)

; new entory時にファイルを作成するディレクトリ
(setq muse-blosxom-base-directory "~/Documents/blogger")
; タグは利用するがディレクトリ分けはしない
;(setq muse-blosxom-use-tags nil)
; new entoryのキー設定
(global-set-key "\C-cpn" 'muse-blosxom-new-entry)
; project設定の中からファイルを検索する設定
(global-set-key "\C-cps" 'muse-project-find-file)
; C-c C-pした時の出力先設定
(setq muse-project-alist
      `(("blog"
         (,@(muse-project-alist-dirs "~/Documents/blogger")
          :default "index")
         ,@(muse-project-alist-styles "~/Documents/blogger"
                                      "~/Documents/blogger/final"
                                      "blosxom-xhtml")
         )))

;; 編集除外ファイルの設定 
(add-to-list 'muse-ignored-extensions "DS_store")

(setq muse-mode-hook
  '(lambda ()
     (setq outline-regexp "*+")
     (setq outline-minor-mode t)))

(provide 'init_muse)
;;; init_muse.el ends here
