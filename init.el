;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;init.el -- Emacs init setting elisp file

;; Copyright (C) 2010-2012 sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;; デバッグ
(set-variable 'debug-on-error t)
(set-variable 'init-file-debug t)

;; cl-lib 利用前提
(eval-when-compile (require 'cl-lib nil t))

;; path追加、条件分岐系関数
(load (locate-user-emacs-file "site-start.d/init_preface.el"))


;; leaf
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("org"   . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; 以下個別設定

(leaf cus-edit
  :doc "custom-file"
  :custom `((custom-file . ,(locate-user-emacs-file "private/customize.el"))))

(leaf user
  :custom ((user-full-name . "sakito")
           (user-mail-address . "sakito@sakito.com")))

(leaf coding
  :doc "文字コード設定"
  :config
  (set-language-environment  'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8-unix)
  ;; 極力UTF-8とする
  (cond
   (mac-p
    ;; Mac OS X の HFS+ ファイルフォーマットではファイル名は NFD (の様な物)で扱う
    ;; 以下はファイル名を NFC で扱う環境と共同作業等する場合の対処
    (require 'ucs-normalize)
    (setq file-name-coding-system 'utf-8-hfs)
    (setq locale-coding-system 'utf-8-hfs))
   (windows-p
    (setq file-name-coding-system 'sjis)
    (setq locale-coding-system 'utf-8))
   (t
    (setq file-name-coding-system 'utf-8)
    (setq locale-coding-system 'utf-8))))

;; 初期位置
(cd "~/")

(leaf ui
  :doc "UI関連"
  :custom (
           ;; toolbar
           (tool-bar-mode . 0)

           ;; scroll bar
           (toggle-scroll-bar . nil)

           ;; 警告を視覚的にする
           (visible-bell . t)

           ;;起動時のmessageを表示しない
           (inhibit-startup-message . t)

           ;; scratch のメッセージを空にする
           (initial-scratch-message . nil)

           ;; 終了時に聞く
           (use-short-answers . t)
           (confirm-kill-emacs . #'yes-or-no-p)
           ))


(leaf edit
  :doc "編集関連"
  :custom (
           ;; 自動改行
           (auto-fill-mode . nil)
           (fill-column . 300)

           ;; モードラインにライン数、カラム数表示
           (line-number-mode . t)
           (column-number-mode . t)

           ;; 行番号表示
           ;; 行番号幅を最初から確保(がたつき防止)
           (display-line-numbers-width-start . 5)
           (display-line-numbers-grow-only . t)
           (display-line-numbers-minor-tick . 100)

           ;; リージョンを kill-ring に入れないで削除できるようにする
           (delete-selection-mode . t)

           ;; TAB はスペース 4 個ぶんを基本
           (tab-width . 4)
           (indent-tabs-mode . nil)

           ;;  対応するカッコを色表示する
           ;; 特に色をつけなくてもC-M-p、C-M-n を利用すれば対応するカッコ等に移動できる
           (show-paren-mode . t)
           ;; カッコ対応表示のスタイル
           ;; カッコその物に色が付く(デフォルト)
           ;; (show-paren-style . parenthesis)
           ;; カッコ内に色が付く
           ;; (show-paren-style . expression)
           ;; 画面内に収まる場合はカッコのみ、画面外に存在する場合はカッコ内全体に色が付く
           ;; (show-paren-style . mixed)

           ;;動的略語展開で大文字小文字を区別
           (dabbrev-case-fold-search . nil)

           ;;新規行を作成しない(デフォルト設定)
           (next-line-add-newlines . nil)

           ;; スクロールのマージン
           (scroll-conservatively . 10000)
           ;; scroll-conservatively の古いバージョン。一行ずつスクロールする
           (scroll-step . 1)
           ;; カーソル位置を変更しない
           (scroll-preserve-screen-position . t)
           ;; shell-mode において最後の行ができるだけウィンドウの一番下にくるようにする
           (comint-scroll-show-maximum-output . t)
  )
  :config 
  (global-display-line-numbers-mode)
  :hook (
         (text-mode-hook . turn-off-auto-fill)
         )
  :bind (
         ;; help key変更
         ("\M-?" . help-for-help)
         ;; BackSpaceをC-hに変更
         ("\C-h" . backward-delete-char)
         ))


(leaf backup
  :custom `(
           ;; ファイルを編集した場合コピーにてバックアップする
           ;; inode 番号を変更しない
           (backup-by-copying . t)

           ;; バックアップファイルの保存位置指定
           ;; !path!to!file-name~ で保存される
           (backup-directory-alist . '(
               ("^/etc/" . ,(locate-user-emacs-file "var/etc"))
               ("." . ,(locate-user-emacs-file "var/emacs"))
               (,tramp-file-name-regexp . nil)))
       ))

(leaf server
  :doc "emacsclient を利用するためにサーバ起動
サーバが起動していた場合は先に起動していた方を優先"
  :require t
  :defun (server-running-p)
  :config
  (unless (server-running-p) (server-start))
  (defun skt:raise-frame()
    ;; Frame を前面にする
    (raise-frame (selected-frame))
    ;; キーボードフォーカスを選択しているFrameにする
    (x-focus-frame (selected-frame)))
  :hook (
         (server-visit-hook . skt:raise-frame)
         (find-file-hook . skt:raise-frame)))


;; 全環境共通設定
(require 'init_main)

;; ;; 終了時バイトコンパイル
;; (add-hook 'kill-emacs-query-functions
;;           (lambda ()
;;             (if (file-newer-than-file-p
;;                  (expand-file-name "init.el" user-emacs-directory)
;;                  (expand-file-name "init.elc" user-emacs-directory))
;;                 (byte-compile-file
;;                  (expand-file-name "init.el" user-emacs-directory)))
;;             (byte-recompile-directory
;;              (expand-file-name "site-start.d" user-emacs-directory) 0)
;;             ))

;; 起動時間計測 目標は常に 3000ms 圏内(dump-emacs すれば可能だがしてない)
(defun message-startup-time ()
  (message "Emacs loaded in %dms"
           (/ (- (+ (cl-third after-init-time)
                    (* 1000000 (cl-second after-init-time)))
                 (+ (cl-third before-init-time)
                    (* 1000000 (cl-second before-init-time))))
              1000)))
(add-hook 'after-init-hook 'message-startup-time)
