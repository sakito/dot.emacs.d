;; init.el -- Emacs init setting elisp file -*- lexical-binding:t -*-

;; Copyright (C) 2010-2024 sakito

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
;; 稼動確認： GUI
;; OS: Mac、Windows WSL Ubuntu、Ubuntu

;; 事前処理
;; git clone https://github.com/axelf4/hotfuzz
;; cmake -B build -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_FLAGS=-march=native
;; cmake --build build
;; cp hotfuzz-module.so ~/.emacs.d/lisp


;; 

;;; Code:

;; デバッグ
(set-variable 'debug-on-error t)
(set-variable 'init-file-debug t)

;; 起動高速化設定
;; file-name-handler-alistを一旦無効にし、起動完了後に戻している
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((my/tmp-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                ;; 起動完了後戻す
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           my/tmp-file-name-handler-alist)))))))

;; gc遅延
;; gcの値を最大にしておき、起動完了後戻す事で起動中のgcを停止する
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; 起動後戻す 8MB程度あればだいたいの場合問題ないはず
            (setq gc-cons-threshold (* 8 1024 1024))))

;; cl-lib 利用前提
(eval-when-compile (require 'cl-lib nil t))

;; cl読み込み停止
;; free-vars、unresolved無視
(eval-and-compile
  (setq byte-compile-warnings t)
  (setq byte-compile-warnings '(not cl-functions free-vars docstrings unresolved))
  )

;; tramp関連停止
(setq tramp-mode nil)




;; leaf
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))


(eval-and-compile
  (customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu"   . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    (leaf hydra :ensure t)
    (leaf blackout :ensure t)

    (leaf el-get :ensure t
      :custom ((el-get-git-shallow-clone  . t)))

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


;; 一部パッケージは強制インストールする必要がある
(when (not (package-installed-p 'orderless))
  (package-install 'orderless))

;; 以下個別設定

(leaf cus-edit
  :doc "custom-file"
  :custom `((custom-file . ,(expand-file-name (concat user-emacs-directory "private/customize.el")))))

(leaf user
  :custom ((user-full-name . "sakito")
           (user-mail-address . "sakito@sakito.com")))


(leaf is_system
  :doc "Emacs の種類バージョンを判別するための変数"
  :init
  (defvar mac-p (or (eq window-system 'mac) (eq window-system 'ns) (eq system-type 'darwin)))
  (defvar windows-p (eq system-type 'windows-nt))
  (defvar linux-p (eq system-type 'gnu/linux))

  (defvar emacs28-p (eq emacs-major-version 28))
  (defvar emacs29-p (eq emacs-major-version 29))
  (defvar emacs30-p (eq emacs-major-version 30))
  (defvar emacs31-p (eq emacs-major-version 31))
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


(leaf exec-path-from-shell
  :url "https://github.com/purcell/exec-path-from-shell"
  :doc "一部環境(wslg、Mac等)で環境変数が正常設定されないのを修正"
  :ensure t
  :when (and window-system (or mac-p))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "LC_ALL"))


(leaf *default-frame
  :when window-system
  :doc "デフォルトのフレーム設定
ディスプレイサイズによって分離する
デュアルだったりトリプルだったりするので width の方は条件に入れてない
設定は (frame-parameter (selected-frame) 'height) などで値を取得して設定する"
  :config
  (leaf display-1440
    :when (= (display-pixel-height) 1440)
    :config
    (setq default-frame-alist
          (append '(
                    (width . 172)
                    (height . 60)
                    (top . 116)
                    (left . 420)
                    (left-fringe . 12)
                    (right-fringe . 12)
                    (alpha . (92 92))
                    )
                  default-frame-alist)))

  (leaf display-1200
    :doc "1920 * 1200 ディスプレイ"
    :when (= (display-pixel-height) 1200)
    :config
    (setq default-frame-alist
          (append '(
                    (width . 175)
                    (height . 65)
                    (top . 50)
                    (left . 500)
                    (alpha . (92 92))
                    )
                  default-frame-alist)))
  )


(leaf font
  :url "https://github.com/yuru7/udev-gothic"
  :when window-system
  :config
  (set-face-attribute 'default
                      nil
                      :family "UDEV Gothic 35NF"
                      :height 180)
  (set-frame-font "UDEV Gothic 35-18")
  (set-fontset-font nil
                    'unicode
                    (font-spec :family "UDEV Gothic 35NF")
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
                    '(#x3000 . #x30ff)
                    (font-spec :family "UDEV Gothic 35NF")
                    nil
                    'prepend)
  ;; 半角カタカナ、全角アルファベット ff00-ffef http://www.triggertek.com/r/unicode/FF00-FFEF
  (set-fontset-font nil
                    '(#xff00 . #xffef)
                    (font-spec :family "UDEV Gothic 35NF")
                    nil
                    'prepend)
  )


(leaf whitespace
  :doc "タブ文字、全角空白、文末の空白の色付け"
  :url "http://www.emacswiki.org/emacs/WhiteSpace"
  :url "http://xahlee.org/emacs/whitespace-mode.html"
  :require t
  :defvar whitespace-style whitespace-display-mappings
  :config
  (setq whitespace-style '(spaces tabs space-mark tab-mark))
  (setq whitespace-display-mappings
        '(
          ;; (space-mark 32 [183] [46]) ; normal space, ·
          (space-mark 160 [164] [95])
          (space-mark 2208 [2212] [95])
          (space-mark 2336 [2340] [95])
          (space-mark 3616 [3620] [95])
          (space-mark 3872 [3876] [95])
          ;; (space-mark ?\x3000 [?\□]) ;; 全角スペース
          ;; (newline-mark 10 [182 10]) ; newlne, ¶

          ;; (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
          (tab-mark 9 [95 9] [92 9])
          ))

  :hook
  (prog-mode-hook . whitespace-mode)
  (text-mode-hook . whitespace-mode)
  :bind (
         ;; 常に whitespace-mode だと動作が遅くなる場合がある
         ("C-x w" . global-whitespace-mode))
  )


(leaf rainbow-mode
  :doc "色コード可視化"
  :ensure t
  :hook
  css-mode-hook
  emacs-lisp-mode-hook
  lisp-mode-hook
  help-mode-hook
  sass-mode-hook
  scss-mode-hook
  toml-ts-mode-hook
  lua-mode-hook
  web-mode-hook)

(leaf rainbow-delimiters
  :ensure t
  :config

  ;; オリジナル https://github.com/bigos/Pyrulis/blob/master/Emacs/personal.el#L159
  (require 'color)
  (defun hsl-to-hex (h s l)
    "Convert H S L to hex colours."
    (let (rgb)
      (setq rgb (color-hsl-to-rgb h s l))
      (color-rgb-to-hex (nth 0 rgb)
                        (nth 1 rgb)
                        (nth 2 rgb))))

  (defun hex-to-rgb (hex)
    "Convert a 6 digit HEX color to r g b."
    (mapcar #'(lambda (s) (/ (string-to-number s 16) 255.0))
            (list (substring hex 1 3)
                  (substring hex 3 5)
                  (substring hex 5 7))))

  (defun bg-color ()
    "Return COLOR or it's hexvalue."
    (let ((color (face-attribute 'default :background)))
      (if (equal (substring color 0 1) "#")
          color
        (apply 'color-rgb-to-hex (color-name-to-rgb color)))))

  (defun bg-light ()
    "Calculate background brightness."
    (< (color-distance  "white"
                        (bg-color))
       (color-distance  "black"
                        (bg-color))))

  (defun whitespace-line-bg ()
    "Calculate long line highlight depending on background brightness."
    (apply 'color-rgb-to-hex
           (apply 'color-hsl-to-rgb
                  (apply (if (bg-light) 'color-darken-hsl 'color-lighten-hsl)
                         (append
                          (apply 'color-rgb-to-hsl
                                 (hex-to-rgb
                                  (bg-color)))
                          '(7))))))

  (defun bracket-colors ()
    "Calculate the bracket colours based on background."
    (let (hexcolors lightvals)
      (setq lightvals (if (bg-light)
                          (list (list .60 1.0 0.55)
                                (list .30 1.0 0.40)
                                (list .11 1.0 0.55)
                                (list .01 1.0 0.65)
                                (list .75 0.9 0.55)
                                (list .49 0.9 0.40)
                                (list .17 0.9 0.47)
                                (list .05 0.9 0.55))
                        (list (list .70 1.0 0.68)
                              (list .30 1.0 0.40)
                              (list .11 1.0 0.50)
                              (list .01 1.0 0.50)
                              (list .81 0.9 0.55)
                              (list .49 0.9 0.40)
                              (list .17 0.9 0.45)
                              (list .05 0.9 0.45))))
      (dolist (n lightvals)
        (push (apply 'hsl-to-hex n) hexcolors))
      (reverse hexcolors)))

  (defun colorise-brackets ()
    "Apply my own colours to rainbow delimiters."
    (interactive)
    (require 'rainbow-delimiters)
    (custom-set-faces
     ;; change the background but do not let theme to interfere with the foreground
     `(whitespace-line ((t (:background ,(whitespace-line-bg)))))
     ;; or use (list-colors-display)
     `(rainbow-delimiters-depth-1-face ((t (:foreground "#80601f"))))
     `(rainbow-delimiters-depth-2-face ((t (:foreground ,(nth 0 (bracket-colors))))))
     `(rainbow-delimiters-depth-3-face ((t (:foreground ,(nth 1 (bracket-colors))))))
     `(rainbow-delimiters-depth-4-face ((t (:foreground ,(nth 2 (bracket-colors))))))
     `(rainbow-delimiters-depth-5-face ((t (:foreground ,(nth 3 (bracket-colors))))))
     `(rainbow-delimiters-depth-6-face ((t (:foreground ,(nth 4 (bracket-colors))))))
     `(rainbow-delimiters-depth-7-face ((t (:foreground ,(nth 5 (bracket-colors))))))
     `(rainbow-delimiters-depth-8-face ((t (:foreground ,(nth 6 (bracket-colors))))))
     `(rainbow-delimiters-depth-9-face ((t (:foreground ,(nth 7 (bracket-colors))))))
     `(rainbow-delimiters-unmatched-face ((t (:foreground "white" :background "red"))))
     `(highlight ((t (:foreground "#ff0000" :background "#888"))))))

  (colorise-brackets)

  :hook
  prog-mode-hook
  text-mode-hook
  org-mdoe-hook)


(require 'modus-themes)
(leaf modus-themes
  :ensure t
  :custom
  (modus-themes-bold-constructs . nil)
  (modus-themes-italic-constructs . t)
  (modus-themes-region . '(bg-only no-extend))
  :preface
  ;; tab
  (defvar my/mark-tabs-face 'my/mark-tabs-face)
  (defface my/mark-tabs-face
    '((t
       (:foreground "#d00000" :underline t)))
    nil
    :group 'font-lock-highlighting-faces)

  ;; 全角スペース
  (defvar my/mark-whitespace-face 'my/mark-whitespace-face)
  (defface my/mark-whitespace-face
    '((t
       (:background "#9f9690" :foreground "#80601f")))
    nil
    :group 'font-lock-highlighting-faces)

  ;; bracket
  (defvar my/brackets-face 'my/brackets-face)
  (defface my/brackets-face
    '((t
       (:foreground "#80601f")))
    nil
    :group 'font-lock-highlighting-faces)

  ;; operator
  (defvar my/operator-face 'my/operator-face)
  (defface my/operator-face
    '((t
       (:foreground "#6f5500")))
    nil
    :group 'font-lock-highlighting-faces)

  ;; 色付け有効化
  (defun my/font-lock-mode (&optional _ARG)
    (font-lock-add-keywords
     major-mode
     '(
       ("\t" 0 my/mark-tabs-face append)
       ("　" 0 my/mark-whitespace-face append)
       ;;("(\\|)\\|{\\|\\}\\|\\[\\|\\]" 0 my/brackets-face append)
       ("[|!\\.\\+\\=\\&]\\|\\/\\|\\:\\|\\%\\|\\*\\|\\," 0 my/operator-face append)
       )))
  (advice-add 'font-lock-mode :before #'my/font-lock-mode)

  :config
  (load-theme 'modus-operandi-tinted t)

  (leaf *custom-modus-themes
    :after modus-themes
    :defvar bg-main
    :defun modus-themes-with-colors my/modus-themes-custom-faces
    :config
    (defun my/modus-themes-custom-faces (&rest _)
      (modus-themes-with-colors
        (custom-set-faces
         `(trailing-whitespace ((,c :background ,bg-main :underline "SteelBlue")))
         `(skk-show-mode-inline-face ((,c :background ,bg-main)))
         )))

    (my/modus-themes-custom-faces)

    )
  )


(leaf ui
  :doc "UI関連"
  :custom (
           ;; scroll bar
           (toggle-scroll-bar . nil)

           ;; 警告を視覚的にする(画面がフラッシュしたり、アイコンが表示される)
           ;; (visible-bell . t)

           ;; 警告音停止
           ;; 警告関連が停止するので、注意
           (ring-bell-function . 'ignore)

           ;; scratch のメッセージを空にする
           (initial-scratch-message . nil)

           ;; yes or no でなく y or n にする
           (use-short-answers . t)

           ;; 終了時に聞く
           (confirm-kill-emacs . #'yes-or-no-p)
           ))


(leaf *scroll-settings
  :doc "スクロールに関する設定"
  :config
  (setq scroll-preserve-screen-position t) ;; スクロール時カーソル位置維持
  ;; smooth-scroll
  ;; スクロールがスムーズになる
  (leaf smooth-scroll
    :ensure t
    :config
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    (setq scroll-step 1) ;; keyboard scroll one line at a time
    )
  )


(leaf nerd-icons
  :doc "M-x nerd-icons-install-fonts"
  :url "https://github.com/rainstormstudio/nerd-icons.el"
  :ensure t)

(leaf edit
  :doc "編集関連"
  :preface
  (defun my/other-window-or-split ()
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

           ;;  対応するカッコを色表示する
           ;; 特に色をつけなくてもC-M-p、C-M-n を利用すれば対応するカッコ等に移動できる
           (show-paren-mode . t)
           ;; 対応表示時間
           (show-paren-delay . 0)
           ;; カッコ対応表示のスタイル
           ;; カッコその物に色が付く(デフォルト)
           ;; (show-paren-style . parenthesis)
           ;; カッコ内に色が付く
           ;; (show-paren-style . expression)
           ;; 画面内に収まる場合はカッコのみ、画面外に存在する場合はカッコ内全体に色が付く
           (show-paren-style . 'mixed)

           ;;動的略語展開で大文字小文字を区別
           (dabbrev-case-fold-search . nil)

           ;;新規行を作成しない(デフォルト設定)
           (next-line-add-newlines . nil)

           ;; スクロールのマージン
           (scroll-conservatively . 1)
           ;; scroll-conservatively の古いバージョン。一行ずつスクロールする
           (scroll-step . 1)
           (next-screen-context-lines . 10)
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

           ;; タブ幅
           (tab-width . 2)
           (custom-tab-width . 2)
  )
  :config
  (global-display-line-numbers-mode)
  (global-font-lock-mode t)
  (setq font-lock-support-mode 'jit-lock-mode)
  (setq-default tab-width 2)

  ;; help 関連 C-h 停止
  (global-unset-key (kbd "<help> C-h"))
  :hook (
         (text-mode-hook . turn-off-auto-fill)
         (text-mode-hook . display-line-numbers-mode)
         (prog-mode-hook . display-line-numbers-mode)
         (conf-mode-hook . display-line-numbers-mode)
         )
  :bind (
         ;; help key変更
         ("\M-?" . help-for-help)

         ;; C-h 該当機能を別キーに
         ("<f1>" . help-command)

         ;; BackSpaceをC-hに変更
         ("\C-h" . backward-delete-char)

         ;; kill ring 系操作変更
         ("C-w" . kill-ring-save)
         ("M-w" . kill-region)

         ;; home、end割り当て
         ("<home>" . beginning-of-buffer)
         ("<end>" . end-of-buffer)

         ;; C-m は 改行とインデントに割り当て(SKK に取られてしまうから)
         ("C-m" . newline-and-indent)

         ;; window の移動
         ("<C-tab>" . my/other-window-or-split)
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
  :defun my/mac-translate-from-yen-to-backslash
  :init
  ;; 円マークをバックスラッシュに変換
  ;; inline_patch からコピー
  ;; (C) Taiichi Hashimoto <taiichi2@mac.com>
  (defun my/mac-translate-from-yen-to-backslash ()
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
  ;; (mac-auto-ascii-mode t)
  ;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)

  ;; 入力モードを英語に変更
  ;; (setq mac-ts-script-language-on-focus '(0 . 0))

  ;; smooth scroll を on
  (setq mac-mouse-wheel-smooth-scroll t)

  (my/mac-translate-from-yen-to-backslash)
  )


(leaf backup
  :custom `(
            ;; backup~ files 作成しない
            (make-backup-files . nil)
            ;; #autosave# files 作成しない
            (auto-save-default . nil)

           ;; ファイルを編集した場合コピーにてバックアップする
           ;; inode 番号を変更しない
           ;;(backup-by-copying . t)

           ;; バックアップファイルの保存位置指定
           ;; !path!to!file-name~ で保存される
           ;; (backup-directory-alist . '(
           ;;     ("^/etc/" . ,(expand-file-name (concat user-emacs-directory "var/etc")))
           ;;     ("." . ,(expand-file-name (concat user-emacs-directory "var/emacs")))
           ;;     (,tramp-file-name-regexp . nil)))
       ))


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

           ;; モードを入力位置近くに表示
           ;; この設定を有効にすると、バッファ表示が遅くなるので設定していない
           ;; (skk-show-tooltip . t)
           ;; (skk-show-mode-show . t)

           ;; モード文字列
           (skk-latin-mode-string . "＠")
           (skk-hiragana-mode-string . "あ")
           (skk-katakana-mode-string . "ア")
           (skk-jisx0208-latin-mode-string . "Ａ")
           (skk-jisx0201-mode-string . "ｱｲ")
           (skk-abbrev-mode-string . "※")
           )
  :config
  ;; @ を無効にする
  (setq skk-rom-kana-rule-list
      (append skk-rom-kana-rule-list
              '(("@" nil "@"))))

  ;; lisp-interaction-mode での実行は C-c C-j に割り当て elisp の項参照

  (leaf ddskk-posframe
    :ensure t
    :blackout t
    :global-minor-mode t
    :custom (
             (ddskk-posframe-border-width . 2)
             )
    :custom-face (
                  (ddskk-posframe . '((t (:background "#ffcfbf"))))
                  )
    )

  :hook (
         ;; C-x C-fでファイルを開くとSKK
         (find-file-hook . (lambda () (skk-latin-mode t)))
         ;; だいたいのmodeでSKK
         (text-mode-hook . (lambda () (skk-latin-mode t)))
         (prog-mode-hook . (lambda () (skk-latin-mode t)))
         (conf-mode-hook . (lambda () (skk-latin-mode t)))
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

    ;; 保存内容を限定
    (desktop-globals-to-save . '(
                                 search-ring
                                 register-alist
                                 file-name-history))
    (desktop-locals-to-save . '(
                                ;; 順序注意
                                desktop-locals-to-save ;; 先頭記載必須？
                                truncate-lines
                                case-fold-search
                                case-replace))

    (desktop-restore-frames . nil)

    (desktop-lazy-verbose . nil)

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

  ;; https://qiita.com/itiut@github/items/d917eafd6ab255629346
  (defmacro my/with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area and
the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
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
  ;; (run-with-idle-timer (* 5 60) t 'recentf-save-list)
  (run-with-idle-timer (* 5 60) t
                       '(lambda ()
                          (my/with-suppressed-message (recentf-save-list))))

  :hook ((after-init-hook . recentf-mode))
  )


(leaf magit
  :doc "magit"
  :ensure t
  :require sendmail  ;; 暫定対処
  :bind (("C-c g" . magit-status)
         ;; 打鍵ミスするので暫定でこれも設定しておく
         ("C-, g" . magit-status)

         (:magit-mode-map
          ("<C-tab>" . my/other-window-or-split)))
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

           ;; mode-line での switchesの表示長
           (dired-switches-in-mode-line . 7)

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
  ;; dired の sort を拡張
  (setq dired-listing-switches
        "-lhavB --time-style \"+%y-%m-%d %H:%M\" --group-directories-first")
  (defvar my/list-of-dired-switches
    '(
      ;; 標準ソート(ディレクトリは上)
      "-lhavB  --time-style \"+%y-%m-%d %H:%M\" --group-directories-first"
      ;; 更新時刻でソート
      "-lhavBt --time-style \"+%y-%m-%d %H:%M\""
      ;; サイズでソート
      "-lhavBS --time-style \"+%y-%m-%d %H:%M\""
      ;; 拡張子でソート(ディレクトリは上)
      "-lhavBX --time-style \"+%y-%m-%d %H:%M\" --group-directories-first"
      )
    "List of ls switches for dired to cycle among.")

  (defun my/cycle-dired-switches ()
    "Cycle through the list `my/list-of-dired-switches' of switches for ls"
    (interactive)
    (setq my/list-of-dired-switches
          (append (cdr my/list-of-dired-switches)
                  (list (car my/list-of-dired-switches))))
    (dired-sort-other (car my/list-of-dired-switches)))

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
           ("C-x C-j" . skk-mode)
           (:dired-mode-map
            ("o" . dired-omit-mode))))


  (leaf gls
    :when mac-p
    :config
    (let ((gls (executable-find "gls")))
      (when gls
        (setq insert-directory-program gls)
        )))

  (leaf nerd-icons-dired
    :url "https://github.com/rainstormstudio/nerd-icons-dired"
    :ensure t
    :blackout t
    :hook (dired-mode-hook . nerd-icons-dired-mode))

  ;; dired-find-alternate-fileを有効化
  (put 'dired-find-alternate-file 'disabled nil)

  (leaf diredfl
    :ensure t
    :hook (dired-mode-hook . diredfl-global-mode))

  :bind (:dired-mode-map
         ;; RETで新規バッファを作成しないでディレクトリを開く(デフォルトは「a」)
         ("RET" . 'dired-find-alternate-file)
         ;; 「a」を押したときに新規バッファ作成
         ("a" . 'dired-advertised-find-file)
         ;; 「s」を押すとソート順序を順次変更する
         ("s" . my/cycle-dired-switches)
         )
  )


(leaf ignoramus
  :ensure t
  :doc "dired-omit-mode(o) にて on/off する"
  :config
  (ignoramus-setup))


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
    :if (executable-find "pt")
    :ensure t
    :config
    (leaf wgrep-pt
      :ensure t
      :hook ((pt-search-mode-hook . wgrep-pt-setup))
    ))

  (leaf rg
    :if (executable-find "rg")
    :ensure t
    :config
    (leaf wgrep-rg
      :require t
      :hook ((rg-mode-hoo . wgrep-rg-setup)))
    )
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

                ("*helm imenu*" :select t :size 0.2 :align left)
                ("\\`\\*helm.*?\\*\\'" :regexp t :size 0.3 :align t)

                ("*eshell*" :select t :other t)
                ("*Shell Command Output*" :select nil)
                ("\\*Async Shell.*\\*" :regexp t :ignore t)
                ("\\*poporg.*\\*" :regexp t :select t :other t)

                ("*Calendar*" :select t :size 0.3)

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
  :defun my/time-stamp-date
  :init
  ;; 時間(更新日)を挿入する
  (defun my/time-stamp-date ()
    "Retune the current time as a string in Date from."
    (format-time-string "%04Y-%02m-%02d: "))
  (defun my/insert-date nil
    "Insert Date."
    (interactive)
    (insert (my/time-stamp-date)))

  ;; face を調査するための関数
  ;; いろいろ知りたい場合は C-u C-x =
  (defun describe-face-at-point ()
    "Return face used at point."
    (interactive)
    (message "%s" (get-char-property (point) 'face)))

  :bind ("C-c d" . #'my/insert-date)
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


(leaf hl-line-plus
  :url "https://github.com/emacsmirror/hl-line-plus"
  :require hl-line+
  :el-get (hl-line-plus
           :url "https://github.com/emacsmirror/hl-line-plus.git")
  :defun toggle-hl-line-when-idle
  :config
  (toggle-hl-line-when-idle 1)
  )


(leaf puni
  :doc "領域選択機能"
  :ensure t
  :require t
  :config
  (defun my/puni-wrap-single-quote (&optional n)
    (interactive "P")
    (puni-wrap-next-sexps
     (puni--parse-interactive-argument-for-wrap n)
     "'" "'"))
  (defun my/puni-wrap-double-quote (&optional n)
    (interactive "P")
    (puni-wrap-next-sexps
     (puni--parse-interactive-argument-for-wrap n)
     "\"" "\""))
  :hydra
  (hydra-puni
   ;; puni 系は C-c v 割り当て
   (global-map "C-c v")
   "puni"
   ;; リスト周辺
   ("c" puni-mark-list-around-point "list")
   ;; S式周辺
   ("x" puni-mark-sexp-around-point "sexp")
   ;; 選択領域拡張
   ("v" puni-expand-region "expand")

   ("[" puni-wrap-square "wrap []")
   ("{" puni-wrap-curly "wrap {}")
   ("(" puni-wrap-round "wrap ()")
   ("<" puni-wrap-angle "wrap <")
   ("'" my/puni-wrap-single-quote)
   ("\"" my/puni-wrap-double-quote)

   ("l" puni-slurp-forward "slurp")
   ("a" puni-barf-forward "barf")
   ("d" puni-splice "splice")

   (")" puni-raise "raise")
  ))

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
  (defun my/smartchr-common ()
    ;; !! がカーソルの位置
    (local-set-key (kbd "[") (smartchr '("[`!!']" "[ [`!!'] ]" "[")))
    (local-set-key (kbd "{") (smartchr '("{`!!'}" "{\n`!!'\n}" "{")))
    (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
    (local-set-key (kbd "`") (smartchr '("\``!!''" "\`")))
    (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
    (local-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'" "\'\'\'`!!'\'\'\'")))
    )

  (defun my/smartchr-clang ()
    (my/smartchr-common)

    (local-set-key (kbd ">") (smartchr '(">" " => " " => '`!!''" " => \"`!!'\"")))
    (local-set-key (kbd ":") (smartchr '(":: " ":")))
    (local-set-key (kbd ";") (smartchr '(";" ";;")))
    )

  (defun my/smartchr-py ()
    (my/smartchr-common)

    (local-set-key (kbd ">") (smartchr '(">" ">>>" " => " " => '`!!''" " => \"`!!'\"")))
    (local-set-key (kbd "#") (smartchr '("# " "### " "#")))
    (local-set-key (kbd "=") (smartchr '("=" " == " " = ")))
    (local-set-key (kbd "+") (smartchr '("+" " + " " += 1")))
    (local-set-key (kbd "-") (smartchr '("-" " - " " -= 1")))

    ;; 上書き
    (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"" "\"\"\"`!!'\"\"\"")))
    )

  (defun my/smartchr-rust ()
    (my/smartchr-common)

    (local-set-key (kbd ":") (smartchr '("::" ":")))
    (local-set-key (kbd ";") (smartchr '(";" ";;")))
    )

  (defun my/smartchr-rst ()
    (my/smartchr-common)

    (local-set-key (kbd ">") (smartchr '(">" ">>>" " => " " => '`!!''" " => \"`!!'\"")))
    (local-set-key (kbd ".") (smartchr '("." ".. ")))

    ;; 上書き
    (local-set-key (kbd "`") (smartchr '("\`\``!!'\`\`" "\``!!'\`" "\'")))
    )

  (defun my/smartchr-md ()
    (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
    (local-set-key (kbd "[") (smartchr '("[`!!']" "[")))
    (local-set-key (kbd "{") (smartchr '("{`!!'}" "{")))
    (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
    (local-set-key (kbd "`") (smartchr '("\`\`\`\n`!!'\n\`\`\`" "\``!!'\`" "\'")))
    )

  (defun my/smartchr-ts ()
    (my/smartchr-common)

    (local-set-key (kbd ":") (smartchr '(":" ":: ")))
    (local-set-key (kbd ";") (smartchr '(";" ";;")))
    (local-set-key (kbd "/") (smartchr '("/" "// `!!'")))
    )

  (defun my/smartchr-lua ()
    (my/smartchr-common)

    (local-set-key (kbd "#") (smartchr '("-- `!!'" "#")))
    )

  (defun my/smartchr-el ()
    (my/smartchr-common)

    (local-set-key (kbd ";") (smartchr '(";;" ";")))
    )

  (defun my/smartchr-skelton ()
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
    (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
    (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
    )

  ;; 適用するモードを限定
  (dolist (hook (list
                 'prog-mode-hook
                 'text-mode-hook
                 'org-mode-hook
                 ))
    (add-hook hook 'my/smartchr-common))

  (dolist (hook (list
                 'makefile-mode-hook
                 'toml-ts-mode-hook
                 ))
    (add-hook hook 'my/smartchr-skelton))

  :hook (
         ;; モードオリジナル追加設定
         (lisp-mode-hook . my/smartchr-el)
         (emacs-lisp-mode-hook . my/smartchr-el)
         (python-mode-hook . my/smartchr-py)
         (python-ts-mode-hook . my/smartchr-py)
         (rust-mode-hook . my/smartchr-rust)
         (rust-ts-mode-hook . my/smartchr-rust)
         (rst-mode-hook . my/smartchr-rst)
         (markdown-mode-hook . my/smartchr-md)
         (c-mode-common-hook . my/smartchr-clang)
         (lua-mode-hook . my/smartchr-lua)
         (typescript-mode-hook . my/smartchr-ts)
         ;; (typescript-ts-mode-hook . my/smartchr-ts)
         )
  )


(leaf vc
  :doc "VCS"
  :custom (
           ;; Gitのみ無効
           ;; (vc-handled-backends . (delq 'Git vc-handled-backends))
           ;; status無効
           (vc-display-status . nil)
           (vc-consult-headers . nil)
           ;; シンボリックリンク先がバージョン管理されていても確認しないでリンク先の実ファイルを開く
           (vc-follow-symlinks . t))
  :hook
  ;; log-edit で メッセージの挿入を停止
  (log-edit-hook . '(log-edit-insert-cvs-templat
                     log-edit-insert-changelog
                     log-edit-show-files)))


(leaf scratch-log
  :doc "scratch バッファを保存する"
  :require t
  :ensure t
  :custom `(
            (sl-scratch-log-file
             . ,(expand-file-name (concat user-emacs-directory "var/scratch.log")))
            (sl-prev-scratch-string-file
             . ,(expand-file-name (concat user-emacs-directory "var/scratch-prev.log")))
            )
  )


(leaf flycheck
  :doc "挙動がおかしい場合は、 M-x flycheck-verify-setup で確認"
  :ensure t
  :custom
  (flycheck-help-echo-function . nil)
  (flycheck-display-errors-function . nil)

  (flycheck-check-syntax-automatically . '(save mode-enabled))
  :config
  ;; python-pylint を停止
  (setq-default flycheck-disabled-checkers '(python-pylint python-flake8 python-mypy))

  (leaf flycheck-posframe
    :ensure t
    :after flycheck
    :hook
    (flycheck-mode-hook . flycheck-posframe-mode)
    :custom
    ;; 右下に表示
    ;; エラー内容の詳細は helm で確認
    (flycheck-posframe-position . 'window-bottom-right-corner)
    ;; エラー表示文字表示する
    ;; アイコンも設定してみたが、文字の方が自分に合ってたので文字にした
    (flycheck-posframe-error-prefix . "[E] ")
    (flycheck-posframe-warning-prefix . "[W] ")
    (flycheck-posframe-info-prefix . "[I] ")
    )

  :hook
  (prog-mode-hook . flycheck-mode))

(leaf helm
  :doc "helm
TODO 一部設定未整備

helmは最新にすると起動しない場合がある
特定のバージョンを取得する場合は以下のようにする
git pull --tags
git checkout v4.0
make
"
  :url "https://github.com/emacs-helm/helm"
  :el-get (helm
           :url "https://github.com/emacs-helm/helm.git"
           :branch "v4.0")
  :blackout t
  :require helm
  :global-minor-mode t
  :custom (
           ;; M-x を保存
           (helm-M-x-always-save-history . t)

           (helm-display-function . 'pop-to-buffer)

           ;; helm-miniの内容
           (helm-mini-default-sources
            . '(
                ;; helm-source-flycheck
                helm-source-buffers-list
                helm-source-file-name-history
                helm-source-recentf
                helm-source-files-in-current-dir
                ;; helm-source-emacs-commands-history
                ;; helm-source-emacs-commands
                helm-source-bookmarks
                ))

           ;; helm-for-filesの内容
           (helm-for-files-preferred-list
            . '(
                helm-source-buffers-list
                helm-source-recentf
                helm-source-file-cache
                helm-ghq-source
                helm-source-files-in-current-dir
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

         ;; find files
         ;; ("C-x C-f" . find-file-at-point)
         ("C-x C-f" . helm-find-files)

         (:helm-map
          ("C-;" .  abort-recursive-edit)
          ;; C-h で削除を有効に
          ("C-h" . delete-backward-char)

          ;; helm-imenu 挙動対応
          ("<f8>" . helm-keyboard-quit)

          ;; TAB に補完的挙動割り当て
          ("TAB" . helm-execute-persistent-action)
          ("<tab>" . helm-execute-persistent-action)
          ;; 元々TABにある機能を C-i に移動
          ("C-i" . helm-select-action)
          )

         ;; helm-imenuを多用していたので、key設定
         (:prog-mode-map
          ;; imenu
          ("C-c i" . helm-imenu)
          ("<f8>" . helm-imenu)
          ("C-c ;" . comment-dwim)
          ("C-c :" . comment-dwim))
         (:text-mode-map
          ;; imenu
          ("C-c i" . helm-imenu)
          ("<f8>" . helm-imenu)
          ("C-c ;" . comment-dwim)
          ("C-c :" . comment-dwim))
         (:dired-mode-map
          ("<f8>" . helm-find-files))
         )
  :defun helm-build-sync-source helm-stringify
  :config
  ;; TODO Invalid function: helm-build-sync-source が発生する場合がある
  ;; コマンド候補
  ;; http://emacs.stackexchange.com/questions/13539/helm-adding-helm-m-x-to-helm-sources
  ;; 上記を参考にして、履歴に保存されるように修正
  ;; (defvar helm-source-emacs-commands
  ;;  (helm-build-sync-source "Emacs commands"
  ;;    :candidates (lambda ()
  ;;                  (let (commands)
  ;;                    (mapatoms (lambda (cmds)
  ;;                                (if (commandp cmds)
  ;;                                    (push (symbol-name cmds)
  ;;                                          commands))))
  ;;                    (sort commands 'string-lessp)))
  ;;    :coerce #'intern-soft
  ;;    :action (lambda (cmd-or-name)
  ;;              (command-execute cmd-or-name 'record)
  ;;              (setq extended-command-history
  ;;                    (cons (helm-stringify cmd-or-name)
  ;;                          (delete (helm-stringify cmd-or-name) extended-command-history)))))
  ;;  "A simple helm source for Emacs commands.")

  ;; (defvar helm-source-emacs-commands-history
  ;;  (helm-build-sync-source "Emacs commands history"
  ;;    :candidates (lambda ()
  ;;                  (let (commands)
  ;;                    (dolist (elem extended-command-history)
  ;;                      (push (intern elem) commands))
  ;;                    commands))
  ;;    :coerce #'intern-soft
  ;;    :action #'command-execute)
  ;;  "Emacs commands history")

  (leaf helm-descbinds
    :url "https://github.com/emacs-helm/helm-descbinds"
    :el-get (helm-descbinds
             :url "https://github.com/emacs-helm/helm-descbinds.git")
    :global-minor-mode t
    )

  (leaf helm-ag
    :url "https://github.com/emacsorphanage/helm-ag"
    :el-get (helm-ag
             :url "https://github.com/emacsorphanage/helm-ag.git")
    :custom (
             (helm-ag-base-command . "pt -e --nocolor --nogroup")
             )
    :after helm
    )

  (leaf helm-flycheck
    :url "https://github.com/yasuyk/helm-flycheck"
    :el-get (helm-flycheck
             :url "https://github.com/yasuyk/helm-flycheck.git")
    :after helm
    :bind (
           ("C-c l" . helm-flycheck)
           ))

  (leaf helm-ghq
    :url "https://github.com/masutaka/emacs-helm-ghq"
    :el-get (helm-ghq
             :url "https://github.com/masutaka/emacs-helm-ghq.git")
    :require helm-for-files helm-ghq
    :after helm)

  :hydra
  (hydra-helm-key
   (global-map "C-c s")
   "helm-key"
   ("." helm-ag "ag")
   ("," helm-ag-pop-stack "pop-stack")
   ("s" helm-do-ag "do-ag")
   ("/" helm-ag-this-file "this-file")
   ("q" helm-ghq "ghq")
   )
  )

(leaf migemo
  :doc "cmigemoコマンド依存"
  :url "https://github.com/koron/cmigemo"
  :ensure t
  :defun migemo-init
  :if (executable-find "cmigemo")
  :require t
  :custom
  (migemo-user-dictionary . nil)
  (migemo-regex-dictionary . nil)
  `(migemo-dictionary . ,(expand-file-name "~/opt/migemo/migemo-dict"))
  :config
  (migemo-init))


(leaf orderless
  :doc "補完候補を空白区切りで複数検索可能にする設定"
  :doc "https://github.com/oantolin/orderless#defining-custom-orderless-styles"
  :defun (migemo-get-pattern . migemo)
  :ensure t
  :config
  (defun orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))
  (eval-when-compile (require 'orderless))
  (orderless-define-completion-style orderless+migemo
                                     (orderless-matching-styles
                                      '(orderless-literal
                                        orderless-regexp
                                        orderless-initialism
                                        orderless-migemo)))
  :custom
  (completion-styles . '(orderless helm basic))
  (orderless-matching-styles
   . '(orderless-literal
       orderless-regexp
       orderless-initialism))
  ;; カテゴリによってcompletion-stylesを変更する
  (completion-category-overrides
   . '((file (styles orderless+migemo partial-completion))
       (buffer (styles orderless+migemo))
       (unicode-name (styles orderless+migemo))
       (kill-ring (styles orderless+migemo))

       ;; eglot
       (eglot (styles orderless+migemo))
       ;; consult with migemo
       ;; (consult-location (styles orderless+migemo)) ; consult-line
       ;; (consult-multi (styles orderless+migemo))    ; consult-buffer
       )))

(leaf corfu
  :leaf-path nil
  :preface
  (leaf hotfuzz
    :ensure t
    :load-path* "lisp"
    :doc "事前にコンパイルが必要 https://github.com/axelf4/hotfuzz"
    )

  (leaf corfu
    :ensure t
    :commands (corfu-quit)
    :init
    (setq completion-ignore-case t)
    ;; インデント済みのときTABキーで補完開始
    (setq tab-always-indent 'complete)
    (defun my/corfu-mode ()
      "Turn on corfu mode."
      (corfu-mode)
      (corfu-popupinfo-mode)
      (corfu-history-mode))
    :custom
    (corfu-cycle . t)
    (corfu-auto . t)

    ;; companyも同時利用する場合は delay 時間を合せる必要がある
    (corfu-auto-delay . 1)
    (corfu-popupinfo-delay . '(0.6 . 0.6))

    ;; corfu中に選択候補をカーソル先に表示しない
    (corfu-preview-current . nil)
    (corfu-auto-prefix . 2)
    (corfu-popupinfo-max-height . 30)
    :custom-face (
                  ;; 色は検討中
                  (corfu-default . '((t (:background "#F2D916"))))
                  )
    :config
    (defun corfu-complete-and-quit ()
      (interactive)
      (corfu-complete)
      (corfu-quit))
    :hook
    ;; corfuではhotfuzzでフィルター/ソートする
    (corfu-mode-hook
     . (lambda () (setq-local completion-styles '(hotfuzz))))
    ;; shellではcorfu起動遅延
    ((shell-mode-hook eshell-mode-hook)
     . (lambda () (setq-local corfu-auto nil) (my/corfu-mode)))
    (prog-mode-hook . my/corfu-mode)
    :bind (
           (:corfu-map
            ;; ("C-f" . corfu-insert)
            ;; ("C-c C-d" . corfu-info-documentation)

            ("C-n" . corfu-next)
            ("C-p" . corfu-previous)

            ([remap completion-at-point] . corfu-complete)
            ("RET" . corfu-complete-and-quit)
            ("<return>" . corfu-complete-and-quit)

            ("C-s" . corfu-insert-separator)
            )
           )
    )

  (leaf nerd-icons-corfu
    :ensure t
    :custom
    (corfu-margin-formatters . '(nerd-icons-corfu-formatter)))

  (leaf cape
    :ensure t
    :doc "バックエンド合成、companyバックエンドの変換"
    :init
    (defun my/elisp-mode-init ()
      "Set completion function to cape"
      (setq-local completion-at-point-functions
                  (list (cape-capf-inside-code
                         (cape-capf-super #'cape-elisp-symbol
                                          #'cape-dabbrev))
                        (cape-capf-inside-string #'cape-file))))

    ;; 現在バッファからの補完
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    ;; path補完
    (add-hook 'completion-at-point-functions #'cape-file)
    ;; keyword補完
    (add-hook 'completion-at-point-functions #'cape-keyword)

    :hook (emacs-lisp-mode-hook . my/elisp-mode-init)
    :bind
    ("C-c o t" . complete-tag)
    ("C-c o d" . cape-dabbrev)
    ("C-c o h" . cape-history)
    ("C-c o f" . cape-file)
    ("C-c o k" . cape-keyword)
    ("C-c o s" . cape-elisp-symbol)
    ("C-c o e" . cape-elisp-block)
    ("C-c o a" . cape-abbrev)
    ("C-c o l" . cape-line)
    ("C-c o w" . cape-dict)
    ("C-c o :" . cape-emoji)
    ("C-c o x" . cape-tex)
    ("C-c o g" . cape-sgml)
    ("C-c o r" . cape-rfc1345))

  (leaf company
    :ensure t
    :doc "capeで既存のcompany補完も利用"
    :custom
    ;; delay 時間等を corfu と合わせないと候補窓が2重に開いてしまう
    (company-idle-delay . 1)
    (company-minimum-prefix-length . 2)
    (company-tooltip-idle-delay . 0)

    (company-dabbrev-ignore-case . t)
    (company-dabbrev-code-ignore-case . t)
    (company-etags-ignore-case . t)
    :bind
    (
     (:company-active-map
      ("C-h" . nil) ;; c-h BackSpace
      )
     )
    )

  (leaf tempel
    :doc "モダンなsnippet補完"
    :ensure t
    :bind ("C-M-o" . tempel-insert))

  (leaf tempel-collection
    :ensure t
    :after tempel)
  )

(leaf yasnippet
  :ensure t
  :blackout yas-minor-mode
  :global-minor-mode yas-global-mode
  :custom
  `(yas-snippet-dirs . '(,(expand-file-name "~/.emacs.d/etc/snippets")))
  :config

  (leaf yasnippet-snippets
    :ensure t)


  (leaf yatemplate
    :doc "auto-insertにyasnippetを利用"
    :url "https://github.com/mineo/yatemplate"
    :ensure t
    :after yasnippet
    :defvar auto-insert-query
    :config
    (setq yatemplate-dir (expand-file-name (concat user-emacs-directory "etc/templates")))
    (auto-insert-mode t)
    (setq auto-insert-query nil)
    (yatemplate-fill-alist)
    )

  (leaf yasnippet-capf
    :doc "yasnippet completion-at-point-functions(capf)"
    :ensure t
    :after yasnippet
    :init
    (add-hook 'completion-at-point-functions #'yasnippet-capf 30 'local)
    )

  )


;; lsp設定
(leaf eglot
  ;; M-x package-install RET eglot RET
  ;; 最新をインストールしないと利用できない場合がある
  :ensure t
  :config
  ;; eglot-server-programs を明確に指定しておく
  (add-hook 'eglot-server-programs

            ;; python-ts-mode で pyright 利用
            ;; uv pip install pyright
            ;; M-! pyright --help が挙動する事
            ;; basedpyright を利用したい場合は pyright を basedpyright に変更
            '((python-mode python-ts-mode) .
              ("pyright-langserver" "--stdio"))

            ;; 参考 https://rust-analyzer.github.io/book/other_editors.html#eglot
            ;; M-! rust-analyzer --help が挙動する事
            '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))

            '((lua-ts-mode lua-mode) .
               ("lua-language-server"))
            )

  ;; eglot無効機能
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider ;; カーソル下のシンボルハイライト
          :inlayHintProvider ;; インラインヒント表示
          ))

  ;; eldoc echo を1行に抑止
  (setq eldoc-echo-area-use-multiline-p nil)
  ;; デフォルトはeldocを停止
  (global-eldoc-mode -1)
  )


(leaf elisp
  :doc "emacs lisp"
  :init
  (defun my/emacs-lisp-hook ()
    (setq indent-tabs-mode nil)
    (local-set-key (kbd "C-c C-c") 'emacs-lisp-byte-compile)
    (local-set-key (kbd "C-c C-r") 'emacs-lisp-byte-compile-and-load)
    (local-set-key (kbd "C-c C-e") 'eval-current-buffer)
    ;; (local-set-key (kbd "C-c C") 'compile-defun)
    (local-set-key (kbd "C-c C-d") 'eval-defun)
    (local-set-key (kbd "C-c f") 'describe-function-at-point)
    (local-set-key (kbd "C-c C-j") 'eval-print-last-sexp)
    (when (fboundp 'expectations)
      ;; C-M-x compile-defun
      (local-set-key (kbd "C-c C-t") 'expectations-execute))
    )
  :hook (
         (lisp-interaction-mode-hook . my/emacs-lisp-hook)
         (emacs-lisp-mode-hook . my/emacs-lisp-hook)
         ))


(leaf editorconfig
  :ensure t
  :global-minor-mode t
  :blackout t)


(leaf treesit
  :doc "tree-sitter設定"
  :require t
  :preface
  (defun treesit-p ()
    "Check if Emacs was built with treesiter in a protable way."
    (and (fboundp 'treesit-available-p)
         (treesit-available-p)))

  (cl-defun treesit-install-and-remap (lang url &key revision source-dir modes remap)
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))
      (unless (treesit-language-available-p lang)
        (add-to-list
         'treesit-language-source-alist
         (list lang url revision source-dir))
        (treesit-install-language-grammar lang))
      (when (and remap (treesit-ready-p lang))
        (dolist (mode modes)
          (add-to-list
           'major-mode-remap-alist
           (cons mode remap))))))

  :config
  ;; M-x treesit-install-language-grammar の候補
  ;; 参考 https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
  ;; 利用環境によってはバージョン指定しないと「version mismatch」が発生する事がある
  (setopt treesit-language-source-alist
    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
      (cmake "https://github.com/uyha/tree-sitter-cmake")
      (css "https://github.com/tree-sitter/tree-sitter-css")
      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
      ;; go は Emacs29.x では v0.19.1にしないと色が付かない
      ;; (go "https://github.com/tree-sitter/tree-sitter-go")
      (go "https://github.com/tree-sitter/tree-sitter-go" "v0.19.1")
      (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
      (html "https://github.com/tree-sitter/tree-sitter-html")
      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
      (json "https://github.com/tree-sitter/tree-sitter-json")
      (make "https://github.com/alemuller/tree-sitter-make")
      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
      (python "https://github.com/tree-sitter/tree-sitter-python")
      (rust "https://github.com/tree-sitter/tree-sitter-rust")
      (toml "https://github.com/tree-sitter/tree-sitter-toml")

      ;; (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2" "tsx/src")
      ;; (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "v0.21.2" "typescript/src")

      ;; (lua "https://github.com/tjdevries/tree-sitter-lua")
      (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  :custom
  (treesit-font-lock-level . 4)
  )


(leaf pythonic
  :ensure t)

(leaf uv-mode
  :ensure t
  :hook (python-base-mode . uv-mode-auto-activate-hook))

(leaf pet
  :ensure t
  :commands (pet-mode)
  :init
  (add-hook 'python-base-mode-hook 'pet-mode -10)
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local python-shell-interpreter (pet-executable-find "python")
                          python-shell-virtualenv-root (pet-virtualenv-root))
              (pet-eglot-setup)
              (pet-flycheck-setup))))

(leaf python-ts-mode
  :mode "\\.py\\'"
  :preface
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

  :init

  (defun my/python-flycheck ()
    ;;(flycheck-add-next-checker 'python-ruff 'python-pyright)
    (flycheck-add-next-checker 'python-ruff 'python-pycompile)
    (flycheck-mode 1)
    )

  :bind ((:python-base-mode-map
          ("C-c !" . run-python)
          ("C-c C-l" . nil)

          ("M-S-<right>" . python-indent-shift-right)
          ("M-S-<left>" . python-indent-shift-left)

          ;; ("C-c n" . flymake-goto-next-error)
          ;; ("C-c p" . flymake-goto-prev-error)
          ;; ("C-c C-i" . my/python-import-modules-from-buffer)
          ;; ("C-c C-c" . my/python-shell-send-file)
          )
         )

  :hydra
  (hydra-py-indent
   (global-map "<f2>")
   "indent"
   (">" python-indent-shift-right "right")
   ("<" python-indent-shift-left "left"))

  :config

  (define-key python-ts-mode-map (kbd "C-c !") 'run-python)

  ;; (leaf flycheck-pycheckers
  ;;   :url "https://github.com/msherry/flycheck-pycheckers"
  ;;   :doc "オリジナルの機能にruffのチェック機能を追加した物を利用"
  ;;   :after flycheck
  ;;   :load-path* "lisp"
  ;;   :require t
  ;;   :custom
  ;;   (flycheck-pycheckers-command . "pycheckers.py")
  ;;   :hook
  ;;   (flycheck-mode-hook . flycheck-pycheckers-setup)
  ;;   )

  :hook (
         ;;(python-ts-mode-hook . eglot-ensure)
         (python-ts-mode-hook . (lambda () (electric-indent-local-mode -1)))
         ;;(python-ts-mode-hook . flycheck-mode)
         (python-ts-mode-hook . my/python-flycheck)
         )
  )


(leaf rust-mode
  :ensure t
  :preface
  (add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
  :custom
  (rust-mode-treesitter-derive . t)
  :hook
  (rust-ts-mode-hook . eglot-ensure)

  :config

  (leaf flycheck-rust
    :ensure t
    :url "https://github.com/flycheck/flycheck-rust/"
    :after flycheck
    :hook
    (flycheck-mode-hook . flycheck-rust-setup)
    )
  )


(leaf c-mode
  :defun c-toggle-hungry-state
  :custom (
           ;; コンパイルメッセージの縦幅
           (compilation-window-height . 8)
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


(leaf typescript-mode
  :mode (
         ("\\.ts\\'" . typescript-mode)
         ;; ("\\.tsx\\'" . tsx-ts-mode)
         )

  :ensure t

  ;; :preface
  ;; (add-to-list 'major-mode-remap-alist '(typescript-mode . typescript-ts-mode))
  )

(leaf json-ts-mode
  :mode "\\.json[c]?\\'"

  :preface
  (add-to-list 'major-mode-remap-alist '(json-mode . json-ts-mode))
)

(leaf go-ts-mode
  :mode "\\.go\\'"

  :preface
  (add-to-list 'major-mode-remap-alist '(go-mode . go-ts-mode))

  :config
  (setq tab-width 2)
  )

(leaf lua-mode
  :when (treesit-p)
  :preface
  :init
  (treesit-install-and-remap
   'lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
  :mode "\\.lua\\'")


(leaf toml-ts-mode
  :require t
  :mode "\\.toml\\'")

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
  :mode "\\.\\(html\\|htm\\)\\'" "\\.js[x]?$"
  :bind (
         (:web-mode-map
          ("C-;" . nil)
          ("C-c C-;" . web-mode-comment-or-uncomment)
          )
         )
  )


(leaf adoc-mode
  :doc "AsciiDoc"
  :url "https://github.com/bbatsov/adoc-mode"
  :ensure t
  :mode "\\.adoc\\'"
  )


(leaf rst
  :require t
  :mode "\\.rst$" "\\.rest$"
  :init
  (defvar rst-html-program "open"
    "Program used to preview HTML files.")
  (defun my/rst-compile-html-preview ()
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
         ("C-c C-p" . my/rst-compile-html-preview)
         )
  :hook (rst-mode-hook . turn-off-auto-fill)
  )


(leaf markdown-mode
  :url "https://github.com/jrblevin/markdown-mode"
  :ensure t
  :mode ("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
         "\\.txt\\'"
         (("README\\.md\\'" . gfm-mode)))

  :custom-face
  (markdown-header-delimiter-face . '((t (:foreground "mediumpurple"))))
  (markdown-header-face-1 . '((t (:foreground "violet" :weight bold))))
  (markdown-header-face-2 . '((t (:foreground "lightslateblue" :weight bold))))
  (markdown-header-face-3 . '((t (:foreground "mediumpurple1" :weight bold))))
  (markdown-link-face . '((t (:foreground "blue" :underline t))))
  (markdown-list-face . '((t (:foreground "mediumpurple"))))
  (markdown-pre-face . '((t (:foreground "MidnightBlue"))))
  )


(leaf protobuf-mode
  :url "https://github.com/protocolbuffers/protobuf"
  :ensure t
  :mode "\\.proto\\'")


(leaf org
  :doc "Emacs添付の物を利用する前提"
  :require t
  :custom `(
            ;; ディレクトリ設定
            (org-directory . ,(expand-file-name "~/Documents/doc/org/"))

            ;; 画像をインラインで表示
            (org-startup-with-inline-images . t)

            ;; TODO状態
            ;; TODO -> RUN -> (待機 WAIT) -> DONE
            ;; SOMEDAY: いずれ、CANCEL: 却下
            (org-todo-keywords . '((sequence "TODO(t)" "READY(r)" "RUN(!)" "WAIT(w)" "NOTE(n)"  "|" "DONE(d)" "SOMEDAY(s)" "CANCEL(c)")))

            ;; DONE時刻記録
            (org-log-done . 'time)

            ;; いろいろやるなら org-agenda-custom-commands 設定推奨
            ;; 予定の表示期間(初期値 week, 日毎なら day)
            ;; 1日前分から表示
            (org-agenda-start-day . "-1d")
            ;; 指定日数(この場合は 14日週間分)表示
            (org-agenda-span . 13)

            ;; 日曜開始
            (org-agenda-start-on-weekday . 0)

            ;; src 内の挙動設定
            ;; font-lock
            (org-src-fontify-natively . t)
            ;; TAB挙動
            (org-src-tab-acts-natively . t)
            ;; src内インデント
            (org-edit-src-content-indentation . 0)
            ;; インデント残す
            (org-src-preserve-indentation . t)
            )
  :config
  (leaf open-junk-file
    :doc "junkに作成 -> 整理して別の場所に移動"
    :ensure t
    :custom `(
              ;; (open-junk-file-format . ,(concat org-directory "/junk/%Y_%m_%d_%H%M%S."))
              (open-junk-file-format . ,(expand-file-name "~/Documents/doc/org/junk/%Y_%m_%d_%H%M%S."))
              )
    :bind
    ("C-c j" . open-junk-file)
    )

  (leaf plain-org-wiki
    :doc "最初から継続調査する事項はwikiを利用している"
    :load-path* "lisp/plain-org-wiki"
    :custom `(
              ;; (plain-org-wiki-directory . ,(concat org-directory "/wiki"))
              (plain-org-wiki-directory . ,(expand-file-name "~/Documents/doc/wiki"))
              )
    :bind
    ("C-c w". plain-org-wiki-helm)
    )

  ;; agenda 設定
  (setopt my/org-tasks-directory (concat org-directory "/agenda/"))

  ;; agenda 以下に当日のorgファイル作成
  (defun my/create-daily-org-file ()
    (interactive)
    (let* ((dir my/org-tasks-directory)
           (path (concat dir (format-time-string "%Y-%m-%d") ".org")))
      (find-file path)
      (save-buffer)
      (setopt org-agenda-files (list path))))

  ;; org-agenda-files として本日、前日、特定ファイルのみを候補とする
  (setopt org-agenda-files
        (let* ((my/today (format-time-string "%Y-%m-%d"))
               (my/prevday (format-time-string "%Y-%m-%d" (time-add (current-time) (* -60 60 24))))
               (my/org-today-file (expand-file-name (concat my/org-tasks-directory my/today ".org")))
               (my/org-prevday-file (expand-file-name (concat my/org-tasks-directory my/prevday ".org"))))
          (list
           (concat my/org-tasks-directory "task.org")
           (if (file-exists-p my/org-today-file)
             my/org-today-file
             (concat my/org-tasks-directory "dummy.org"))
           (if (file-exists-p my/org-prevday-file)
             my/org-prevday-file
             (concat my/org-tasks-directory "dummy.org"))
           )
          ))

  :bind
  ("C-c n" . my/create-daily-org-file)
  ("C-c a" . org-agenda)
  )


;; (leaf which-key
;;   :url "https://github.com/justbur/emacs-which-key"
;;   :ensure t
;;   :config
;;   (which-key-mode))


(leaf mode-line
  :doc "mode-line のフォーマット"
  :config
  (leaf nerd-icons-mode-line
    :ensure t
    :require t
    :vc (:url "https://github.com/grolongo/nerd-icons-mode-line")
    :custom
    ;; default value
    (nerd-icons-mode-line-v-adjust . 0.1)
    ;; default value
    (nerd-icons-mode-line-size . 1.0)
    )

  ;; 改行文字表現変更
  (setq eol-mnemonic-dos "(CRLF)")
  (setq eol-mnemonic-unix "(LF)")
  (setq eol-mnemonic-mac "(CR)")
  (setq eol-mnemonic-undecided "(?)")

  ;; 列、行、割合表示
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

  (defface my/mode-line-warning
    '((((class color))
       (:background "#dfd5cf" :foreground "#d00000")))
    nil
    :group 'face)

  ;; 保存状態
  (setq-default mode-line-modified
                '(:eval
                  (when (and (buffer-file-name)
                             (buffer-modified-p))
                    (list
                     (propertize "  "
                                 'face 'my/mode-line-warning)
                     )))
                )

  ;; 読み取り専用
  (setq-default mode-line-readonly
                '(:eval
                    (when (and (buffer-file-name)
                               buffer-read-only)
                      (list
                       (propertize "  "
                                   'face 'my/mode-line-warning)
                       )))
                )

  ;; flycheck-mode-line
  (setq my/flycheck-mode-line
        '(:eval
          (when
              (and (bound-and-true-p flycheck-mode)
                   (or flycheck-current-errors
                       (eq 'running flycheck-last-status-change)))
            'flycheck-mode-line
          ))
        )

  (setq-default mode-line-format
                (list
                  "--"
                  "" skk-modeline-input-mode "%e"
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-readonly
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-nerd-icon
                  mode-line-buffer-identification
                  mode-line-position
                  'mode-line-modes
                  my/flycheck-mode-line
                  "--"
                  ;; (which-func-mode ("" which-func-format ("--" 0 2)))
                  ;; (global-mode-string ("" global-mode-string))
                  "--"
                  '("-%-" 0 3)
                  ))

  (leaf minions
    :ensure t
    :custom
    (minions-mode-line-lighter . "[+]")
    :config
    (minions-mode))
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
  :doc "起動時間計測 目標は常に 3000ms 圏内"
  :init
  (defun my/message-startup-time ()
    (message "Emacs loaded in %dms"
             (/ (- (+ (cl-third after-init-time)
                      (* 1000000 (cl-second after-init-time)))
                   (+ (cl-third before-init-time)
                      (* 1000000 (cl-second before-init-time))))
                1000)))
  :hook (after-init-hook . my/message-startup-time)
  )


(provide 'init)
;;; init.el ends here
