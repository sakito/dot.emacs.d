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

(setq user-full-name "sakito")
(setq user-mail-address "sakito@sakito.com")

;; 詳細時間計測用
;; (load "~/.emacs.d/private/private/timelag.el")

;; 常時デバッグ状態
(setq debug-on-error t)

;; cl-lib 利用前提
(eval-when-compile (require 'cl-lib nil t))

;; Emacs 設定ディレクトリを設定。Emacs 22以下用
;; Emacs 23.1 以上では user-emacs-directory 変数が用意されているのでそれを利用
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory (expand-file-name "~/.emacs.d/")))

;; 引数を load-path へ追加
;; normal-top-level-add-subdirs-to-load-path はディレクトリ中で
;; [A-Za-z] で開始する物だけ追加する。
;; 追加したくない物は . や _ を先頭に付与しておけばロードしない
;; dolist は Emacs 21 から標準関数なので積極的に利用して良い
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Emacs Lisp のPathを通す
(add-to-load-path "lisp"
                  ;; 変更したり、自作の Emacs Lisp
                  "local-lisp"
                  ;; private 内には自分専用の物がはいっている
                  ;; 依存は private 内で完結するようにしている
                  "private"
                  ;; 初期設定ファイル
                  "site-start.d")

;; Emacs の種類バージョンを判別するための変数を定義
;; @see http://github.com/elim/dotemacs/blob/master/init.el
(defun x->bool (elt) (not (not elt)))
(defvar emacs27-p (equal emacs-major-version 27))
(defvar emacs28-p (equal emacs-major-version 28))
(defvar emacs29-p (equal emacs-major-version 29))
(defvar darwin-p (eq system-type 'darwin))
(defvar ns-p (featurep 'ns))
(defvar mac-p (and (eq window-system 'mac) (or emacs27-p emacs28-p emacs29-p)))
(defvar linux-p (eq system-type 'gnu/linux))
(defvar nt-p (eq system-type 'windows-nt))
(defvar windows-p (or nt-p))

;; 文字コード
;;(set-language-environment 'Japanese)
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
  (setq locale-coding-system 'utf-8)))

;; 全環境共通設定
(require 'init_global)

;; 環境依存設定
(cond
 (mac-p (require 'init_main))
 (ns-p (require 'init_sysns))
 (t (require 'init_main))
 )

;; custom-file
(setq custom-file
      (expand-file-name "private/customize.el" user-emacs-directory))

;; 終了時バイトコンパイル
(add-hook 'kill-emacs-query-functions
          (lambda ()
            (if (file-newer-than-file-p
                 (expand-file-name "init.el" user-emacs-directory)
                 (expand-file-name "init.elc" user-emacs-directory))
                (byte-compile-file
                 (expand-file-name "init.el" user-emacs-directory)))
            (byte-recompile-directory
             (expand-file-name "site-start.d" user-emacs-directory) 0)
            ))

;; 起動時間計測 目標は常に 3000ms 圏内(dump-emacs すれば可能だがしてない)
(when (or emacs27-p emacs28-p emacs29-p)
  (defun message-startup-time ()
    (message "Emacs loaded in %dms"
             (/ (- (+ (third after-init-time)
                      (* 1000000 (second after-init-time)))
                   (+ (third before-init-time)
                      (* 1000000 (second before-init-time))))
                1000)))
  (add-hook 'after-init-hook 'message-startup-time))
