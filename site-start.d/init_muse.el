;;; init_muse.el --- Emacs Muse setting

;; Copyright (C) 2008  sakito

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

;; howm の設定
;; 以下 howm と Muse の連携設定 howmを必要とする
;; Muse のディレクティブにする
;(setq howm-view-title-header " ")
;(setq howm-view-title-header "*")
;;(setq howm-view-title-header "#title")
;;(setq howm-template-date-format "[%Y-%m-%d %H:%M:%S]")
(require 'howm)
;; elscreen-howm を利用
(require 'elscreen-howm)
;; メニュー表示を日本語とする
(setq howm-menu-lang 'ja)
;; 「最近のメモ」表示にタイトルを表示
(setq howm-list-recent-title t)
;; 全てのメモを表示時にタイトルを表示
(setq howm-list-all-title t)

;; rstでメモを書く
(setq auto-mode-alist
      (append '(
                ("\\.howm$" . rst-mode)
                )
              auto-mode-alist))

;; テンプレートの形式を変更
(setq howm-template
      (concat howm-view-title-header " %title%cursor\n========================================\n\n"))

;; タイトル色
(set-face-foreground 'howm-mode-title-face "OliveDrab1")
;(set-face-foreground 'howm-mode-keyword-face "yellow") ;; <<<
;(set-face-foreground 'howm-mode-ref-face "yellow") ;; >>>
;(set-face-foreground 'action-lock-face "cyan") ;; 下線文字
;(set-face-underline 'action-lock-face t) ;; 下線は文字と同色 (Emacs 21)
;(set-face-underline 'action-lock-face "blue") ;; 下線 (Emacs 21)

;; RET でファイルを開く際, 一覧バッファを消す
;; C-u RET なら残る
(setq howm-view-summary-persistent nil)

;; howm の時は auto-fill にする
;(add-hook 'howm-mode-on-hook 'auto-fill-mode)
;; howm の時は auto-fill にしない
(add-hook 'howm-mode-on-hook 'turn-off-auto-fill)

;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SwitchMemoDirectory
;(defun my-howm-set-directory (dir &optional keyfile)
;  `(lambda ()
;     (interactive)
;     (setq howm-directory ,dir)
;     (when ,keyfile
;       (setq howm-keyword-file ,keyfile))
;     (setq howm-menu-next-expiry-time (current-time))
;     (message "%s" ,dir)))

;; 切り替えてメニューを呼ぶ (thx > [[2ch:619]]さん)
;(defun my-howm-switch-directory (dir &optional keyfile)
;  (funcall (my-howm-set-directory dir keyfile))
;  (howm-menu))

;(global-set-key "\C-c,1" (my-howm-set-directory "~/howm" "~/.howm-keys"))
;(global-set-key "\C-c,2" (my-howm-set-directory "~/Documents/blogger" "~/Documents/blogger/.howm-keys"))


(provide 'init_muse)
;;; init_muse.el ends here
