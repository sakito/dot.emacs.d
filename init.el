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


(leaf is_system
  :doc ";; Emacs の種類バージョンを判別するための変数"
  :init
  (defvar mac-p (and (eq window-system 'mac)))
  (defvar windows-p (eq system-type 'windows-nt))
  (defvar linux-p (eq system-type 'gnu/linux))

  (defvar emacs27-p (equal emacs-major-version 27))
  (defvar emacs28-p (equal emacs-major-version 28))
  (defvar emacs29-p (equal emacs-major-version 29))
  )


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


(leaf *default-frame
  :doc "デフォルトのフレーム設定
ディスプレイサイズによって分離する
デュアルだったりトリプルだったりするので width の方は条件に入れてない
設定は (frame-parameter (selected-frame) 'height) などで値を取得して設定する"
  :config
  (leaf display-1440
    :when (= (display-pixel-height) 1440)
    :config
    (setq default-frame-alist
          (append (list
                   '(width . 172)
                   '(height . 60)
                   '(top . 123)
                   '(left . 420)
                   '(alpha . (92 70))
                   )
                  default-frame-alist)))

  (leaf display-1200
    :doc "1920 * 1200 ディスプレイ"
    :when (= (display-pixel-height) 1200)
    :config
    (setq default-frame-alist
          (append (list
                   '(width . 175)
                   '(height . 65)
                   '(top . 50)
                   '(left . 500)
                   '(alpha . (92 70))
                   )
                  default-frame-alist)))
  )


(leaf font
  :doc "https://github.com/yuru7/Firge"
  :config
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
  ;; 記号        3000-303F http://www.triggertek.com/r/unicode/3000-303F
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
  )


(leaf whitespace
  :require t
  :defvar whitespace-style whitespace-display-mappings
  :config
  ;; タブ文字、全角空白、文末の空白の色付け
  ;; @see http://www.emacswiki.org/emacs/WhiteSpace
  ;; @see http://xahlee.org/emacs/whitespace-mode.html
  (setq whitespace-style '(spaces tabs space-mark tab-mark))
  (setq whitespace-display-mappings
        '(
          ;; (space-mark 32 [183] [46]) ; normal space, ·
          (space-mark 160 [164] [95])
          (space-mark 2208 [2212] [95])
          (space-mark 2336 [2340] [95])
          (space-mark 3616 [3620] [95])
          (space-mark 3872 [3876] [95])
          (space-mark ?\x3000 [?\□]) ;; 全角スペース
          ;; (newline-mark 10 [182 10]) ; newlne, ¶
          (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
          ))
  :bind (
         ;; 常に whitespace-mode だと動作が遅くなる場合がある
         ("C-x w" . global-whitespace-mode))
  )


(leaf nightsblue-theme
  :when window-system
  :config
  (add-to-list 'custom-theme-load-path
               (locate-user-emacs-file "theme"))
  (load-theme 'nightsblue t t)
  (enable-theme 'nightsblue)
  )

(leaf ui
  :doc "UI関連"
  :custom (
           ;; scroll bar
           (toggle-scroll-bar . nil)

           ;; 警告を視覚的にする
           (visible-bell . t)

           ;; scratch のメッセージを空にする
           (initial-scratch-message . nil)

           ;; yes or no でなく y or n にする
           (use-short-answers . t)

           ;; 終了時に聞く
           (confirm-kill-emacs . #'yes-or-no-p)
           ))


(leaf edit
  :doc "編集関連"
  :preface
  (defun other-window-or-split ()
    (interactive)
    (when (one-window-p)
      (split-window-horizontally))
    (other-window 1))
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

           ;; CUA-mode にて矩形選択のみを有効化
           (cua-enable-cua-keys . nil)
           (cua-mode . t)

           ;; 行末の空白を表示
           (show-trailing-whitespace . t)

           ;; EOB を表示
           (indicate-empty-lines . t)
           (indicate-buffer-boundaries . 'left)

           ;; マーク領域を色付け
           (transient-mark-mode . t)
  )
  :config
  (global-display-line-numbers-mode)
  (global-font-lock-mode t)
  (setq font-lock-support-mode 'jit-lock-mode)
  :hook (
         (text-mode-hook . turn-off-auto-fill)
         (prog-mode-hook . display-line-numbers-mode)
         )
  :bind (
         ;; help key変更
         ("\M-?" . help-for-help)

         ;; BackSpaceをC-hに変更
         ("\C-h" . backward-delete-char)

         ;; kill ring 系操作変更
         ("C-w" . kill-ring-save)
         ("M-w" . kill-region)

         ;; C-m は 改行とインデントに割り当て(SKK に取られてしまうから)
         ("C-m" . newline-and-indent)

         ;; window の移動
         ("<C-tab>" . other-window-or-split)
         ))


(leaf uniquify
  :doc "同一ファイル識別のためディレクトリ名を付与する"
  :require t
  :custom (
           ;; ディレクトリ名を付与し angle brackets で囲む
           (uniquify-buffer-name-style . 'post-forward-angle-brackets)
           ;; 表示階層の深さ
           (uniquify-min-dir-content . 2)
           ))


(leaf mac
  :doc "mac用の設定"
  :when mac-p
  :defun mac-translate-from-yen-to-backslash
  :init
  ;; 円マークをバックスラッシュに変換
  ;; inline_patch からコピー
  ;; (C) Taiichi Hashimoto <taiichi2@mac.com>
  (defun mac-translate-from-yen-to-backslash ()
    ;; Convert yen to backslash for JIS keyboard.
    (interactive)

    (define-key global-map [165] nil)
    (define-key global-map [2213] nil)
    (define-key global-map [3420] nil)
    (define-key global-map [67109029] nil)
    (define-key global-map [67111077] nil)
    (define-key global-map [8388773] nil)
    (define-key global-map [134219941] nil)
    (define-key global-map [75497596] nil)
    (define-key global-map [201328805] nil)
    (define-key function-key-map [165] [?\\])
    (define-key function-key-map [2213] [?\\]) ;; for Intel
    (define-key function-key-map [3420] [?\\]) ;; for PowerPC
    (define-key function-key-map [67109029] [?\C-\\])
    (define-key function-key-map [67111077] [?\C-\\])
    (define-key function-key-map [8388773] [?\M-\\])
    (define-key function-key-map [134219941] [?\M-\\])
    (define-key function-key-map [75497596] [?\C-\M-\\])
    (define-key function-key-map [201328805] [?\C-\M-\\])
    )
  :config
  ;; インプッットメソッドの設定
  ;; (setq default-input-method "MacOSX-IM-JP")
  ;; インプットメソッド対応パッチにてctrキーをOS側に渡さない設定
  ;; (mac-add-ignore-shortcut '(control))
  ;; システムに装飾キー渡さない
  (setq mac-pass-control-to-system nil)
  (setq mac-pass-command-to-system nil)
  ;; (setq mac-pass-option-to-system nil)

  ;;コマンドキーをMetaキーとして利用
  ;; (setq mac-command-key-is-meta t)
  (if (eq mac-option-modifier nil)
      (progn
        (setq mac-option-modifier 'meta)
        (setq mac-command-modifier 'hyper)
        )
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  ;; (setq mac-command-key-is-meta nil)
  ;; (setq ns-command-modifier (quote meta))

  ;; システムの IM を無視する
  ;; (setq mac-use-input-method-on-system nil)
  ;; 起動したら US にする
  ;; (add-hook 'after-init-hook 'mac-change-language-to-us)
  ;; minibuffer 内は US にする
  (mac-auto-ascii-mode t)
  ;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)

  ;; 入力モードを英語に変更
  ;; (setq mac-ts-script-language-on-focus '(0 . 0))

  ;; smooth scroll を on
  (setq mac-mouse-wheel-smooth-scroll t)

  (mac-translate-from-yen-to-backslash)
  )


(leaf highlight
  :defvar highlight-changes-visibility-initial-state
  :config
  ;; 変更点に色付け
  (global-highlight-changes-mode t)
  ;; 初期は非表示として highlight-changes-visible-mode で表示する
  (setq highlight-changes-visibility-initial-state nil)
  :bind (
         ("M-]" . highlight-changes-next-change)
         ("M-[" . highlight-changes-previous-change)
         )
  )


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
  :defvar skk-rom-kana-rule-list
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
  :require elscreen-server elscreen-dired
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
  :defvar recentf-keep
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
  :require sendmail  ;; 暫定対処
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)

         (:magit-mode-map
          ("<C-tab>" . other-window-or-split)))
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


(leaf dired
  :doc "dired"
  :require t
  :custom `(
           ;; 再帰コピー
           (dired-recursive-copies . 'always)
           ;; 再帰削除
           ;; (dired-recursive-deletes . 'always)

           ;; C-x 2 で分割した隣にコピーや移動をする
           (dired-dwim-target . t)

           ;; dired-x の機能を利用して 特定ファイルだけ「!」や「X」でQuick Look 可能にする
           ;; QL の終了は C-g
           (dired-guess-shell-alist-user
            . '(("\\.png$" "qlmanage -p")
                ("\\.jpg$" "qlmanage -p")
                ("\\.pdf$" "open")))
           )
  :config
  ;; dired 上で r を押すと wdired-change-to-wdired-mode を動作させる
  (leaf wdired
    :require t
    :bind (:dired-mode-map
           ("r" . wdired-change-to-wdired-mode))
    )

  ;; dired-x を起動
  (leaf dired-x
    :require t
    :bind (
           ;; dired-x では C-x C-j がdired-jump になるので skk-modeに再割り当て
           ("C-x C-j" . skk-mode)))

  ;; s で並び変え、C-u s で元に戻る
  ;; @see sorter.el
  (leaf sorter
    :load-path* "lisp"
    :require t)

  ;; システムのlsでなくls-lispを利用して表示
  (leaf ls-lisp
    :require t
    :custom ((ls-lisp-use-insert-directory-program . nil)
             ;; ls のオプション
             (dired-listing-switches . "-lahF")
             ;; ディレクトリをより上に表示
             (ls-lisp-dirs-first . t)
             ))

  ;; dired-find-alternate-fileを有効化
  (put 'dired-find-alternate-file 'disabled nil)

  :bind (:dired-mode-map
         ;; RETで新規バッファを作成しないでディレクトリを開く(デフォルトは「a」)
         ("RET" . 'dired-find-alternate-file)
         ;; 「a」を押したときに新規バッファ作成
         ("a" . 'dired-advertised-find-file))
  )


(leaf grep
  :config
  (leaf wgrep
    :url "https://github.com/mhayashi1120/Emacs-wgrep"
    :ensure t
    :custom (
             (wgrep-enable-key . "r")
             )
    )
  (leaf pt
    :url "https://github.com/monochromegane/the_platinum_searcher"
    :url "https://github.com/bling/pt.el"
    :ensure t
    :config
    (leaf wgrep-pt
      :ensure t
      :hook ((pt-search-mode-hook . wgrep-pt-setup))
    ))
  )

(leaf ffap
  :require t
  :custom (
           (ffap-c-path
            . '("/opt/local/include" "/usr/include" "/usr/local/include"))

           ;; 新規ファイルの場合には確認する
           (ffap-newfile-prompt . t)

           ;; ffap-kpathsea-expand-path で展開するパスの深さ
           (ffap-kpathsea-depth . 5)
           )
  :bind (
         ;; C-x C-f
         ("C-c C-f" . find-file-at-point)
         )
  )


(leaf shackle
  :ensure t
  :global-minor-mode t
  :custom (
           ;; default nil
           (shackle-select-reused-windows . nil)

           ;;default below
           (shackle-default-alignment . 'below)

           ;; default 0.5
           (shackle-default-size . 0.4)

           (shackle-rules
            . '((compilation-mode :select nil)
                ("*Completions*" :size 0.3  :align t)
                ("*Messages*" :select nil :inhibit-window-quit t :other t)
                ("*Compile-Log*" :size 10 :select nil)

                ("*Help*" :select t :inhibit-window-quit t :other t)
                ("*info*" :select t :inhibit-window-quit t :same t)
                ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t)

                ("\\`\\*helm.*?\\*\\'" :regexp t :size 0.3 :align t)

                ("*eshell*" :select t :other t)
                ("*Shell Command Output*" :select nil)
                ("\\*Async Shell.*\\*" :regexp t :ignore t)
                ("\\*poporg.*\\*" :regexp t :select t :other t)

                ("*Calendar*" :select t :size 0.3 :align below)

                ("*aHg diff*" :sise 50 :align above :select t)
                ("*aHg log*" :align left)
                ("\\*hg command" :regexp t :select nil)

                (magit-status-mode :select t :inhibit-window-quit t :same t)
                (magit-log-mode :select t :inhibit-window-quit t :same t)
                ))
           )
  )


(leaf function
  :doc "独自関数"
  :defun time-stamp-date
  :init
  ;; 時間(更新日)を挿入する
  (defun time-stamp-date ()
    "Retune the current time as a string in Date from."
    (format-time-string "%04Y-%02m-%02d: "))
  (defun insert-date nil
    "Insert Date."
    (interactive)
    (insert (time-stamp-date)))

  ;; face を調査するための関数
  ;; いろいろ知りたい場合は C-u C-x =
  (defun describe-face-at-point ()
    "Return face used at point."
    (interactive)
    (message "%s" (get-char-property (point) 'face)))

  :bind ("C-c d" . #'insert-date)
  )


(leaf calendar
  :require t
  :custom (
           ;; week number
           (calendar-intermonth-text
            . '(propertize
                (format "%02dW"
                        (car
                         (calendar-iso-from-absolute
                          (calendar-absolute-from-gregorian
                           (list month (- day (1- calendar-week-start-day)) year)))))
                'font-lock-face 'calendar-iso-week-face))
           )
  :bind (
         (:calendar-mode-map
          ("f" . calendar-forward-day)
          ("n" . calendar-forward-day)
          ("b" . calendar-backward-day))
         )

  :config
  ;; https://github.com/emacs-jp/japanese-holidays
  (leaf japanese-holidays
    :ensure t
    :require t
    :after calendar
    :defvar calendar-holidays japanese-holidays calendar-mark-holidays-flag
    :config
    ;; 他の国の祝日も表示させたい場合は append を追加
    (setq calendar-holidays
          (append japanese-holidays holiday-local-holidays holiday-other-holidays))

    ;; 祝日をカレンダーに表示
    (setq calendar-mark-holidays-flag t)
    :hook
    ;; 今日をマークする
    (calendar-today-visible-hook . calendar-mark-today)
    (calendar-today-visible-hook . japanese-holiday-mark-weekend)
    (calendar-today-invisible-hook . japanese-holiday-mark-weekend)
  ))


(leaf autoinsert
  :require t
  :global-minor-mode auto-insert-mode
  :defvar auto-insert-alist template-replacements-alists
  :init
  ;; 置換用の関数
  (defun my-template ()
    (time-stamp)
    (mapc #'(lambda(c)
              (progn
                (goto-char (point-min))
                (replace-string (cl-first c) (funcall (cl-rest c)) nil)))
          template-replacements-alists)
    (goto-char (point-max))
    (message "done."))
  :custom `(
           ;; テンプレートとなるファイルがあるディレクトリ
           ;; 末尾に"/"が必要なので注意
           (auto-insert-directory
            . ,(expand-file-name "etc/autoinsert/" user-emacs-directory))

           ;; 質問しないで auto-insertを実行する
           (auto-insert-query . nil))

  :config
  ;; テンプレート用の置換文字列
  (defvar template-replacements-alists
    '(
      ("%file%" . (lambda () (file-name-nondirectory (buffer-file-name))))
      ("%module%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      ;;("%time%" . (lambda () (format-time-string "%Y-%m-%d %k:%M:%S" (current-time))))
      ("%time%" . (lambda () (format-time-string "%Y-%m-%d 00:00:00" (current-time))))
      ("%year%" . (lambda () (format-time-string "%Y" (current-time))))
      ;; ("%file-without-ext%" . (lambda () (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
      ;; ("%include-guard%" . (lambda () (format "__SCHEME_%s__" (upcase (file-name-sans-extension (file-name-nondirectory buffer-file-name))))))
      ))

  ;; 各モードの設定
  ;; Python
  (setq auto-insert-alist
        (nconc '(
                 ("\\.rst$" . ["rst.rst" my-template])
                 (python-mode . ["python.py" my-template])
                 ) auto-insert-alist))
  ;; Lisp
  (setq auto-insert-alist
        (nconc '(
                 ("\\.cl$" . ["cl.lisp" my-template])
                 ("\\.lisp$" . ["cl.lisp" my-template])
                 (lisp-mode . ["cl.lisp" my-template])
                 ) auto-insert-alist))

  ;; Shell
  (setq auto-insert-alist
        (nconc '(
                 ("\\.sh$" . ["shell.sh" my-template])
                 (sh-mode . ["shell.sh" my-template])
                 ) auto-insert-alist))
  :hook (
         ;; ファイルを開いたら実行
         (find-file-hook . auto-insert)
         (find-file-not-found-hooks . auto-insert))
  )


(leaf hl-line-plus
  :url "https://github.com/emacsmirror/hl-line-plus"
  :require hl-line+
  :el-get (hl-line-plus
           :url "https://github.com/emacsmirror/hl-line-plus.git")
  :defun toggle-hl-line-when-idle
  :config
  (toggle-hl-line-when-idle 1)
  )


(leaf smartchr
  :doc "smartchr の設定"
  :url "http://tech.kayac.com/archive/emacs-tips-smartchr.html"
  :require t
  :el-get (smartchr
           :url "https://github.com/imakado/emacs-smartchr.git")
  :defun smartchr
  :defvar skeleton-pair skeleton-pair-on-word skeleton-end-hook
  :config
  ;; 無名関数だと add-hook や remove-hook がめんどいのでまとめておく
  (defun smartchr-custom-keybindings ()
    ;; !! がカーソルの位置
    (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
    (local-set-key (kbd "[") (smartchr '("[`!!']" "[ [`!!'] ]" "[")))
    (local-set-key (kbd "{") (smartchr '("{`!!'}" "{\n`!!'\n}" "{")))
    (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
    (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
    (local-set-key (kbd ">") (smartchr '(">" " => " " => '`!!''" " => \"`!!'\"")))
    (local-set-key (kbd ";") (smartchr '(";; " ";")))
    )

  (defun smartchr-custom-keybindings-clang ()
    ;; !! がカーソルの位置
    (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
    (local-set-key (kbd "[") (smartchr '("[`!!']" "[ [`!!'] ]" "[")))
    (local-set-key (kbd "{") (smartchr '("{`!!'}" "{\n`!!'\n}" "{")))
    (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
    (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
    (local-set-key (kbd ">") (smartchr '(">" " => " " => '`!!''" " => \"`!!'\"")))
    (local-set-key (kbd ":") (smartchr '(":: " ":")))
    (local-set-key (kbd ";") (smartchr '(";" ";;")))
    )

  (defun smartchr-custom-keybindings-py ()
    (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
    (local-set-key (kbd "[") (smartchr '("[`!!']" "[ [`!!'] ]" "[")))
    (local-set-key (kbd "{") (smartchr '("{`!!'}" "{\n`!!'\n}" "{")))
    (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
    (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")))
    (local-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'" "\'\'\'`!!'\'\'\'")))
    (local-set-key (kbd ">") (smartchr '(">" ">>>" " => " " => '`!!''" " => \"`!!'\"")))
    (local-set-key (kbd "#") (smartchr '("# " "### " "#")))
    (local-set-key (kbd "=") (smartchr '("=" " == " " = ")))
    (local-set-key (kbd "+") (smartchr '("+" " + " " += 1")))
    (local-set-key (kbd "-") (smartchr '("-" " - " " -= 1")))
    )

  (defun smartchr-custom-keybindings-rst ()
    (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
    (local-set-key (kbd "[") (smartchr '("[`!!']" "[ [`!!'] ]" "[")))
    (local-set-key (kbd "{") (smartchr '("{\n`!!'\n}" "{`!!'}" "{")))
    (local-set-key (kbd "`") (smartchr '("\`\``!!'\`\`" "\``!!'\`" "\'")))
    (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
    (local-set-key (kbd ">") (smartchr '(">" ">>>" " => " " => '`!!''" " => \"`!!'\"")))
    (local-set-key (kbd ".") (smartchr '("." ".. ")))
    )

  (defun skelton-custom-keybindigs ()
    ;;   (make-variable-buffer-local 'skeleton-pair)
    ;;  (make-variable-buffer-local 'skeleton-pair-on-word)
    ;;  (make-variable-buffer-local 'skeleton-pair-alist)
    (setq skeleton-pair t)
    (setq skeleton-pair-on-word t)
    (setq skeleton-end-hook nil)
    (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
    (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
    (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
    (local-set-key (kbd "`") 'skeleton-pair-insert-maybe)
    (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
    )

  ;; 適用するモードを限定
  (dolist (hook (list
                 'css-mode-hook
                 'js2-mode-hook
                 'lisp-mode-hook
                 'emacs-lisp-mode-hook
                 'sql-mode-hook
                 ))
    (add-hook hook 'smartchr-custom-keybindings))

  (dolist (hook (list
                 'makefile-mode-hook
                 ))
    (add-hook hook 'skelton-custom-keybindigs))

  :hook (
         ;; モードオリジナル追加設定
         (python-mode-hook . smartchr-custom-keybindings-py)
         (rst-mode-hook . smartchr-custom-keybindings-rst)
         (c-mode-common-hook . smartchr-custom-keybindings-clang))
  )


(leaf vc
  :doc "VCS"
  :custom (
           (vc-handled-backends . nil)
           (vc-display-status . nil)
           (vc-consult-headers . nil)
           ;; シンボリックリンク先がバージョン管理されていても確認しないでリンク先の実ファイルを開く
           (vc-follow-symlinks . t))
  :hook
  ;; log-edit で メッセージの挿入を停止
  (log-edit-hook . '(log-edit-insert-cvs-template
                     log-edit-insert-changelog
                     log-edit-show-files)))


(leaf scratch-log
  :doc "scratch バッファを保存する"
  :require t
  :ensure t
  :custom `(
            (sl-scratch-log-file
             . ,(expand-file-name "var/scratch.log" user-emacs-directory))
            (sl-prev-scratch-string-file
             . ,(expand-file-name "var/scratch-prev.log" user-emacs-directory))
            )
  )


(leaf company
  :ensure t
  :global-minor-mode global-company-mode
  :custom
  (company-transformers . '(company-sort-by-backend-importance))
  (company-idle-delay . 0)
  (company-echo-delay . 0)
  ;; 開始文字数
  (company-minimum-prefix-length . 2)
  (company-selection-wrap-around . t)
  (completion-ignore-case . t)
  :bind
  (
   (:company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("C-s" . company-filter-candidates)
    ("C-i" . company-complete-selection))
   (:company-search-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous))
   )
  :config
  (leaf company-posframe
    :ensure t
    :require t)
  )


(leaf flycheck
  :ensure t
  :hook (prog-mode-hook . flycheck-mode))


(leaf elisp
  :doc "emacs lisp"
  :init
  (defun skt:emacs-lisp-hook ()
    (setq indent-tabs-mode nil)
    (local-set-key (kbd "C-c C-c") 'emacs-lisp-byte-compile)
    (local-set-key (kbd "C-c C-r") 'emacs-lisp-byte-compile-and-load)
    (local-set-key (kbd "C-c C-e") 'eval-current-buffer)
    ;; (local-set-key (kbd "C-c C") 'compile-defun)
    (local-set-key (kbd "C-c C-d") 'eval-defun)
    (local-set-key (kbd "C-c ;") 'comment-dwim)
    (local-set-key (kbd "C-c :") 'comment-dwim)
    (local-set-key (kbd "C-c f") 'describe-function-at-point)
    (when (fboundp 'expectations)
      ;; C-M-x compile-defun
      (local-set-key (kbd "C-c C-t") 'expectations-execute))
    )
  :hook (
         (lisp-interaction-mode-hook . skt:emacs-lisp-hook)
         (emacs-lisp-mode-hook . skt:emacs-lisp-hook)
         ))


(leaf python
  :require t
  :mode "\\.wsgi\\'" "wscript"
  :init
  ;; env
  (setenv "PYTHONSTARTUP"
          (expand-file-name "rc.d/pythonrc.py" user-emacs-directory))
  (setenv "PYTHONPATH"
          (expand-file-name "~/opt/py3.12.1/lib/python3.12/site-packages"))

  :bind (:python-mode-map
         ("C-c ;" . comment-dwim)
         ("C-c :". comment-dwim)
         ("C-c !" . run-python)
         ("C-c C-l" . nil)

         ;; ("C-c n" . flymake-goto-next-error)
         ;; ("C-c p" . flymake-goto-prev-error)
         ;; ("C-c C-i" . skt:python-import-modules-from-buffer)
         ;; ("C-c C-c" . skt:python-shell-send-file)
         )

  :hook (
         (python-mode-hook . (lambda () (electric-indent-local-mode -1)))
         (python-mode-hook . (lambda () (company-mode -1)))
         (python-mode-hook . (lambda () (company-posframe-mode -1)))
         )

  :config
  (leaf cython-mode :ensure t)

  (leaf python-flycheck
    :require flycheck
    :config
    (flycheck-define-checker python-pylintrunner
      "lintrunner.py"
      :command ("lintrunner.py" source-inplace)
      :error-patterns
      ((error line-start
              "ERROR " (optional (id (one-or-more (not (any ":"))))) ":"
              (message) " at " (file-name) " line " line (optional "," column) "." line-end)
       (warning line-start
                "WARNING " (optional (id (one-or-more (not (any ":"))))) ":"
                (message) " at " (file-name) " line " line (optional "," column) "." line-end))
      :modes python-mode)
    (add-to-list 'flycheck-checkers 'python-pylintrunner)

    :hook (python-mode-hook . flycheck-mode)
    )
  )


(leaf c-mode
  :defun c-toggle-hungry-state
  :custom (
           ;; コンパイルセッセージの縦幅
           (compilation-window-height . 8)

           ;; 基本オフセット
           (c-basic-offset . 2)
           )
  :hook (
         (c-mode-common-hook
          . (lambda()
             ;; styleには GNU,cc-mode,ktr,bsd,stroustrup,whitesmith
             ;; ,ellemtel,linux等がある
             (c-set-style "cc-mode")

             ;; namespace {}の中はインデントしない
             (c-set-offset 'innamespace 0)

             ;; 連続するスペースをバックスペース一回で削除する
             (c-toggle-hungry-state t)
             ))
         )
  )


(leaf makefile-mode
  :hook (
         (makefile-mode-hook
          .  (lambda ()
               (whitespace-mode t)
               ;; suspicious-lines を無視しておく
               (fset 'makefile-warn-suspicious-lines 'ignore)
               (setq indent-tabs-mode t)))
         )
  )


(leaf web-mode
  :ensure t
  :mode "\\.\\(html\\|htm\\)\\'"
  :custom (
           ;; html
           (web-mode-markup-indent-offset . 2)
           ;; css
           (web-mode-css-indent-offset . 2)
           ;; js, php, etc..
           (web-mode-code-indent-offset . 2)
           (web-mode-comment-style . 2)
           )
  :bind (
         (:web-mode-map
          ("C-;" . nil)
          ("C-c C-;" . web-mode-comment-or-uncomment)
          )
         )
  )


(leaf css-mode
  :require t
  :mode "\\.css\\'"
  :init
  (defun hexcolour-luminance (color)
    "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
    (let* ((values (x-color-values color))
           (r (car values))
           (g (cadr values))
           (b (caddr values)))
      (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))

  (defun hexcolour-add-to-font-lock ()
    (interactive)
    (font-lock-add-keywords nil
                            `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
                                        (regexp-opt (x-defined-colors) 'words))
                               (0 (let ((colour (match-string-no-properties 0)))
                                    (put-text-property
                                     (match-beginning 0) (match-end 0)
                                     'face `((:foreground ,(if (> 128.0 (hexcolour-luminance colour))
                                                               "white" "black"))
                                             (:background ,colour)))))))))
  :hook (css-mode-hook . hexcolour-add-to-font-lock)
  )


(leaf adoc-mode
  :doc "AsciiDoc"
  :url "https://github.com/bbatsov/adoc-mode"
  :ensure t
  :mode "\\.txt\\'"
  )


(leaf rst
  :require t
  :mode "\\.rst$" "\\.rest$"
  :init
  (defvar rst-html-program "open"
    "Program used to preview HTML files.")
  (defun rst-compile-html-preview ()
    "Convert the document to a HTML file and launch a preview program."
    (interactive)
    (let* ((tmp-filename "/tmp/out.html")
           (command (format "rst2html.py --template %s/etc/rst/blog_template.txt --stylesheet-path %s/etc/rst/sourcecode.css %s %s && %s %s"
                            user-emacs-directory user-emacs-directory
                            buffer-file-name tmp-filename
                            rst-html-program tmp-filename)))
      (start-process-shell-command "rst-html-preview" nil command)
      ))
  :bind (:rst-mode-map
         ("C-c C-c" . rst-compile)
         ("C-c C-p" . rst-compile-html-preview)
         ("C-c ;" . comment-dwim)
         ("C-c :" . comment-dwim)
         )
  :hook (rst-mode-hook . turn-off-auto-fill)
  )


(leaf markdown-mode
  :url "https://github.com/jrblevin/markdown-mode"
  :ensure t
  :mode ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
         (("README\\.md\\'" . gfm-mode)))
  )



(leaf helm
  :doc "helm
TODO 一部設定未整備"
  :url "https://github.com/emacs-helm/helm"
  :ensure t
  :require helm helm-autoloads
  :global-minor-mode t
  :custom (
           ;; M-x を保存
           (helm-M-x-always-save-history . t)

           (helm-display-function . 'pop-to-buffer)

           (helm-mini-default-sources
            . '(
                ;; helm-source-flycheck
                helm-source-buffers-list
                helm-source-file-name-history
                helm-source-recentf
                helm-source-files-in-current-dir
                helm-source-emacs-commands-history
                helm-source-emacs-commands
                helm-source-bookmarks
                ))
           )
  :bind (
         ;; mini buffer 起動
         ("C-;" . helm-mini)

         ;; コマンド表示
         ("M-x" . helm-M-x)

         ;; バッファ切り替え時の一覧表示
         ("C-x C-b" . helm-for-files)

         ;; kill ring
         ("M-y". helm-show-kill-ring)

         ;; C-x C-f には helm 無効
         ;; ("C-c C-f" . find-file-at-point)

         ;; imenu
         ("C-c i" . helm-imenu)

         (:helm-map
          ("C-;" .  abort-recursive-edit)
          ;; C-h で削除を有効に
          ("C-h" . delete-backward-char))
         )
  :defun helm-build-sync-source helm-stringify
  :config
  ;; TODO Invalid function: helm-build-sync-source が発生する場合があるので、ここでも require している
  (require 'helm)
  (require 'helm-autoloads)
  ;; コマンド候補
  ;; http://emacs.stackexchange.com/questions/13539/helm-adding-helm-m-x-to-helm-sources
  ;; 上記を参考にして、履歴に保存されるように修正
  (defvar helm-source-emacs-commands
   (helm-build-sync-source "Emacs commands"
     :candidates (lambda ()
                   (let (commands)
                     (mapatoms (lambda (cmds)
                                 (if (commandp cmds)
                                     (push (symbol-name cmds)
                                           commands))))
                     (sort commands 'string-lessp)))
     :coerce #'intern-soft
     :action (lambda (cmd-or-name)
               (command-execute cmd-or-name 'record)
               (setq extended-command-history
                     (cons (helm-stringify cmd-or-name)
                           (delete (helm-stringify cmd-or-name) extended-command-history)))))
   "A simple helm source for Emacs commands.")

  (defvar helm-source-emacs-commands-history
   (helm-build-sync-source "Emacs commands history"
     :candidates (lambda ()
                   (let (commands)
                     (dolist (elem extended-command-history)
                       (push (intern elem) commands))
                     commands))
     :coerce #'intern-soft
     :action #'command-execute)
   "Emacs commands history")

  (leaf helm-descbinds
    :ensure t
    :global-minor-mode t
    )

  (leaf helm-ag
    :ensure t
    :custom (
             (helm-ag-base-command . "pt -e --nocolor --nogroup")
             )
    :bind (
           ("M-g ." . helm-ag)
           ("M-g ," . helm-ag-pop-stack)
           ("M-g s" . helm-do-ag)
           ("C-M-s" . helm-ag-this-file)
           )
    )

  (leaf helm-flycheck
    :ensure t
    :bind (
           ("C-c l" . helm-flycheck)
           ))
  )


(leaf mode-line
  :doc "mode-line のフォーマット"
  :config
  (setq-default mode-line-position
                '(:eval
                  (list
                   "  ["
                   (propertize "%03l" 'face 'font-lock-type-face)
                   "/"
                   (propertize (format "%d" (count-lines (point-max) (point-min))) 'face 'font-lock-type-face)
                   "("
                   (propertize "%02p" 'face 'font-lock-type-face)
                   ")"
                   ","
                   (propertize "%03c" 'face 'font-lock-type-face)
                   "] "
                   ))
                )

  (setq-default mode-line-format
                '(
                  (elscreen-display-screen-number ("-" elscreen-e21-mode-line-string))
                  "" skk-modeline-input-mode "%e"
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  mode-line-position
                  mode-line-modes
                  "--"
                  (which-func-mode ("" which-func-format ("--" 0 2)))
                  (global-mode-string ("" global-mode-string))
                  "--"
                  ("-%-" 0 3)
                  ))
  )


(leaf private
  :doc "非公開系
private 内には自分専用の物がはいっている
依存は private 内で完結するようにしている"
  :when mac-p
  :load-path* "private"
  :config
  (require 'init_private))


(leaf startup-time
  :doc "起動時間計測 目標は常に 3000ms 圏内(dump-emacs すれば可能だがしてない)"
  :init
  (defun message-startup-time ()
    (message "Emacs loaded in %dms"
             (/ (- (+ (cl-third after-init-time)
                      (* 1000000 (cl-second after-init-time)))
                   (+ (cl-third before-init-time)
                      (* 1000000 (cl-second before-init-time))))
                1000)))
  :hook (after-init-hook . message-startup-time)
  )


(provide 'init)
;;; init.el ends here
