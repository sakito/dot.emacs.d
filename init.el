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
(require 'sendmail)
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

(leaf setenv
  :doc "一部環境で環境変数が正常設定されないので、設定する
不要な物もあるかもしれない"
  :config
  ;; LC_ALL
  (setenv "LC_ALL" "ja_JP.UTF-8")

  ;; PATH設定
  ;; Mac OS X の bash の PATH は /usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/X11/bin:
  ;; 多数の実行環境にて極力汎用的にパスが設定されるようしたい
  (dolist (dir (list
                "/sbin"
                "/usr/sbin"
                "/bin"
                "/usr/bin"
                "/opt/homebrew/bin"
                "/usr/local/bin"
                "/usr/texbin"
                (expand-file-name "~/bin")
                (expand-file-name "~/opt/py3.11/bin")
                (expand-file-name "bin" user-emacs-directory)
                ))
    (when (and (file-exists-p dir) (not (member dir exec-path)))
      (setenv "PATH" (concat dir ":" (getenv "PATH")))
      (setq exec-path (append (list dir) exec-path))))

  (setenv "CVS_RSH" "ssh")
  (setenv "DISPLAY" "localhost")
  (setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))
  )

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


(leaf skk
  :doc "ddskk設定"
  :ensure ddskk
  :require t
  :custom (
           ;; C-\ でも SKK に切り替えられるように設定
           (default-input-method . "japanese-skk")

           ;; 送り仮名が厳密に正しい候補を優先して表示
           (skk-henkan-strict-okuri-precedence . t)

           ;; 漢字登録時、送り仮名が厳密に正しいかをチェック
           (skk-check-okurigana-on-touroku . t)

           ;; skk server設定
           (skk-server-host . "localhost")
           (skk-server-portnum . 1178)

           ;; カーソル色
           (skk-cursor-hiragana-color . "hot pink")
           )
  :config
  ;; @ を無効にする
  (setq skk-rom-kana-rule-list
      (append skk-rom-kana-rule-list
              '(("@" nil "@"))))

  (defadvice skk-latin-mode (after no-latin-mode-in-lisp-interaction activate)
    "`lisp-interaction-mode' において英数モードを回避する。"
    (when (eq major-mode 'lisp-interaction-mode)
      (skk-mode-off)))

  (leaf ddskk-posframe
    :ensure t
    :global-minor-mode t
    :custom ((ddskk-posframe-border-width . 2))
    )
  :hook (
         ;; C-x C-fでファイルを開くとSKK
         (find-file-hook . (lambda () (skk-latin-mode t)))
         )
  )


(leaf elscreen
  :doc "elscreen"
  :ensure t
  :require elscreen-server
  :custom
  (dnd-open-file-other-window . nil)
  :hook (after-init-hook . elscreen-start)
  )


(leaf desktop
  :doc "状態保存"
  :global-minor-mode desktop-save-mode
  :custom
  `(
    ;; 保存場所を変更
    (desktop-base-file-name
     . ,(expand-file-name "var/session/desktop" user-emacs-directory))
    (desktop-base-lock-name
     . ,(expand-file-name "var/session/desktop.lock" user-emacs-directory))

    ;; 保存間隔(初期値は30秒)
    (desktop-auto-save-timeout . ,(* 5 60))
  ))


(leaf savehist
  :doc "ヒストリー保存"
  :global-minor-mode savehist-mode
  :custom
  `(
    ;; 保存場所を変更
    (savehist-file
     . ,(expand-file-name "var/session/savehist" user-emacs-directory))

    ;; ミニバッファ履歴リストの長さ制限を無くす
    (history-length . t)

    ;; 重複除去
    (history-delete-duplicates . t)

    ;; ミニバッファの履歴保存
    (savehist-save-minibuffer-history . t)
  )
  :config
  ;; history、ring系全部保存
  (setopt savehist-additional-variables
        (apropos-internal "-\\(\\(history\\)\\|\\(ring\\)\\)\\'" 'boundp))
  )


(leaf recentf
  :doc "recentf"
  :require t
  :init
  (leaf recentf-ext
    :ensure t)
  :custom
  `(
    ;; recentf ファイルの保存場所を指定。デフォルトはホームの直下
    (recentf-save-file
     . ,(expand-file-name "var/recentf.cache" user-emacs-directory))

    ;; 自動クリーニングを停止 recentf-cleanup
    ;; tramp や 外部ディスクを利用している場合停止しておかないと面倒な動作になる
    (recentf-auto-cleanup . 'never)

    ;; 履歴の保存量を多少多めにしておく
    (recentf-max-saved-items . 1000)

    ;; 除外ファイル
    (recentf-exclude
     . '("\\.elc$"
         "\\.pyc$"
         "\\.cache$"
         ".recentf$"
         ".howm-keys$"
         "^/var/folders/"
         "^/tmp/"))
    )
  :config
  ;; 保存ファイルの設定に リモートファイル tramp の先等を追加。これを実施すると起動時にパスワード等の確認はされない
  (add-to-list 'recentf-keep 'file-remote-p)
  (add-to-list 'recentf-keep 'file-readable-p)

  ;; 一定の未使用時間毎に自動保存
  (run-with-idle-timer (* 5 60) t 'recentf-save-list)

  :hook ((after-init-hook . recentf-mode))
  )


(leaf magit
  :doc "magit"
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :init
  (leaf transient
    :custom
    `((transient-levels-file
       . ,(expand-file-name "var/transient/levels.el" user-emacs-directory))
      (transient-history-file
       . ,(expand-file-name "var/transient/history.el" user-emacs-directory))
      (transient-values-file
       . ,(expand-file-name "var/transient/values.el" user-emacs-directory))
      (transient-force-fixed-pitch . t))
    ))

;; 移行前設定
(require 'init_dired)
(require 'init_wgrep)

;; 操作
;; (require 'init_recentf)
(require 'init_key)
(require 'init_helm)
(require 'init_shackle)
(require 'init_function)
(require 'init_calendar)

;; 開発
(require 'init_autoinsert)
(require 'init_smartchr)
;(require 'init_scm)
(require 'init_lisp)
(require 'init_modeinfo)
(require 'init_python)
(require 'init_c)
(require 'init_web-mode)
(require 'init_css)

;; テキストファイル
(require 'init_adoc)
(require 'init_rst)
(require 'init_markdown)

;; フレームサイズ、色、フォントの設定
(require 'init_font)
(require 'init_color)
(require 'init_modeline)

;; 非公開系
(when mac-p
  (require 'init_private))


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
