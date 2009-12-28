;;emacs23.el -- emacs23 init setting elisp file

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

;;; 初期位置
(cd "~/")

;; ログの長さを無限に
;;(setq message-log-max 't)
;; ログを出さない
;; (setq message-log-max nil)

;;; menubar
;(menu-bar-mode nil)
;;; toolbar
(tool-bar-mode -1)

(setq user-full-name "sakito")
(setq user-mail-address "sakito@sakito.com")

;; ファイルを編集した場合コピーにてバックアップする
;; inode 番号を変更しない
(setq backup-by-copying t)
;;; バックアップファイルの保存位置指定[2002/03/02]
;; !path!to!file-name~ で保存される
(setq backup-directory-alist
      '(
        ("^/etc/" . "~/.emacs.d/var/etc")
        ("." . "~/.emacs.d/var/emacs")
        ))

;; Emacs-Lisp のPathを通す
;; normal-top-level-add-subdirs-to-load-path はディレクトリ中の中で
;; [A-Za-z] で開始する物だけ追加するので、追加したくない物は . や _ を先頭に付与しておけばロードしない
(let ((default-directory (expand-file-name "~/.emacs.d/lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(let ((default-directory (expand-file-name "~/.emacs.d/local-lisp")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-start.d"))

;; 文字コード
(set-language-environment 'Japanese)
;; 極力UTF-8とする
(prefer-coding-system 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; フレームサイズ、色、フォントの設定
(require 'init_color)

;;; 環境変数関連の設定
(require 'init_setenv)

;; emacsclient
(server-start)


; 印刷の設定
(setq ps-multibyte-buffer 'non-latin-printer)

; 自動改行関連
(setq-default auto-fill-mode nil)
(setq-default fill-column 300)
(setq text-mode-hook 'turn-off-auto-fill)

; 削除ファイルをOSのごみ箱へ
;(setq delete-by-moving-to-trash t)

;;; help key変更
;; BackSpaceをC-hに変更
;(load-library "obsolete/keyswap")
(global-set-key "\M-?" 'help-for-help)
;; keyswap は obsoleteなので以下の設定が良い
(global-set-key "\C-h" 'backward-delete-char)

;;:eshell
;; glob で .* が .. に一致しないようにする
(setq eshell-glob-include-dot-dot nil)

;;; Shellの設定
;; M-x shell
;; @see http://home7.highway.ne.jp/dayan/tips/mac/bash.html
;(setq shell-file-name "/bin/zsh")
(setq shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
;(setq explicit-bash-args '("-login" "-i"))
;(setq shell-command-switch "-c")
;(setq win32-quote-process-args t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 編集設定
;(load "physical-line")
(require 'init_key)

;; TAB はスペース 4 個ぶんを基本
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; モードラインにライン数、カラム数表示
(line-number-mode 1)
(column-number-mode 1)

;; 対応するカッコを色表示する
(show-paren-mode 1)

;; モードラインにファイルのディレクトリを表示
;(add-to-list 'global-mode-string '("" default-directory "-"))

;;動的略語展開で大文字小文字を区別
(setq dabbrev-case-fold-search nil)

;;新規行を作成しない
;;emacs21ではデフォルトで設定されています。
(setq next-line-add-newlines nil)

;;起動時のmessageを表示しない
(setq inhibit-startup-message t)

;; スクロールのマージン
;; 一行ずつスクロールする
(setq scroll-conservatively 35)
(setq scroll-margin 0)
(setq scroll-step 1)
(setq comint-scroll-show-maximum-output t)
;(setq next-screen-context-lines 3)

;; 終了時に聞く
(setq confirm-kill-emacs 'y-or-n-p)

;; vc はすばらしいがわたしは利用しないので無効にする
(setq vc-handled-backends nil)
(setq vc-ignore-vc-files t)
;; シンボリックリンク先がバージョン管理されていても確認しないでリンク先の実ファイルを開く
(setq vc-follow-symlinks t)

;;; Elscreen
(require 'init_elscreen)

;; dired
(require 'init_dired)

;;; SKK の設定
(require 'init_skk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 独自関数
(require 'init_function)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; プログラミング関連
;;; womanの設定
(require 'init_woman)
;;;; mode-info
(require 'init_modeinfo)
;;; Lisp [2004/04/17]
(require 'init_lisp)
;;; Pythonの設定
(require 'init_python)
;;; Gaucheの設定
(require 'init_gauche)
;;; JDEE [2002/06/25]
;(require 'init_jdee)
;;;; sql-mode
(require 'init_sql)
;;; mmm-mode [2003/10/12]現在不使用
;(require 'init_mmm)
;;; smart-compie
(require 'init_smartcompile)

;; ecb
;(require 'init_ecb)
;; c
(require 'init_c)
;; yasnippet
(require 'init_yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SCM関連
;; Subversion Mode
;; @see http://xsteve.nit.at/prg/vc_svn/
(require 'psvn)
;; dvc
;(load-file "~/.emacs.d/lisp/dvc-snapshot/dvc-load.el")
;(setq dvc-prefix-key [(control c) ?h])
; hg-mode
(require 'ahg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 補完関連
(require 'ac-mode)
;; 常にac-modeをONにする
(add-hook 'find-file-hooks 'ac-mode-without-exception)

(require 'init_moccur)

(require 'init_eldoc)

;; anything
(require 'init_anything)

;; session
(require 'init_session)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 文書記述関連

;;; auto-insert
(require 'init_autoinsert)
;;; htmlize
;; @see http://fly.srk.fer.hr/~hniksic/emacs/
(require 'htmlize)
;;; muse + howm
(require 'init_muse)

;;; sdicの設定
;(require 'init_sdic)

;; Dictionary.app 呼びだし
(require 'init_adic)
(define-key global-map "\C-cw" 'ite-dict-func)

;;; AUC TeX
;(require 'init_auctex)
;;; htmlhelperの設定
(require 'init_html)
;;; css-modeの設定
(require 'init_css)
;;;; javascript-mode
(require 'init_javascript)
;;; nxml-mode
;; @http://www.thaiopensource.com/download/
(require 'init_nxml)
;;;; hns-mode
;(require 'init_hnf)
;; bhl mode
;;(require 'init_bhl)
;; rst-mode
(require 'init_rst)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/po"))
(load "start-po")
(eval-after-load 'po-mode '(load "gb-po-mode"))

;(require 'sense-region)
;(defadvice set-mark-command (around sense-region-set-mark-23 activate)
;  (if (and (mell-transient-region-active-p)
;           sense-region-mode)
;      (copy-face 'region 'sense-region-region-face))
;  ad-do-it)
;(sense-region-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ネットワーク関連
;; wl
;(require 'init_wl)
;; w3m
;(require 'init_w3m)
;; navi2ch
(require 'init_navi2ch)
;; pukiwiki
;(require 'init_pukiwiki)
;; irc
;(require 'init_irc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テスト中,確認中
(add-to-list 'load-path (expand-file-name "~/dev/applescript-mode"))
(autoload 'applescript-mode "applescript-mode" "AppleScript Mode." t)
;(require 'applescript-mode)
(setq auto-mode-alist
      (cons '("\\.applescript$" . applescript-mode) auto-mode-alist)
      )
(setq auto-mode-alist
      (cons '("\\.as$" . applescript-mode) auto-mode-alist)
      )

;; d-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/d-mode"))
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(setq auto-mode-alist
      (cons '( "\\.d\\'" . d-mode) auto-mode-alist)
      )

;; hatena-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/hatena-mode"))
(load "hatena-mode")
(setq hatena-usrid "sakito")
(setq hatena-plugin-directory "~/var/hatena/plugin/")
(setq hatena-entry-type 0)
;(setq hatena-directory (expand-file-name "~/var/hatena/"))

;; mac-screencapture
(add-to-list 'load-path (expand-file-name "~/Sites/develop/screencapture/"))
(require 'mac-screencapture)
(setq mac-screencapture-schemes
  '(
    ("current-directory-images"
              :dir "images")
    ("local"
     :dir "~/images/")
    ("current-directory"
     :dir default-directory)
    ))
(setq mac-screencapture-default-scheme "current-directory")
(define-key global-map "\C-cp" 'mac-screencapture)

;; private 内には自分専用の物がはいっている。依存は private 内で完結するようにしている
(let ((default-directory (expand-file-name "~/.emacs.d/private")))
  (add-to-list 'load-path default-directory)
  (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
      (normal-top-level-add-subdirs-to-load-path)))
(autoload 'seizon "seizon-mode" nil t)

;;end emacs23.el
