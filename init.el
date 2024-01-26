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

;; user
(setq user-full-name "sakito")
(setq user-mail-address "sakito@sakito.com")

;; custom-file
(setq custom-file
      (expand-file-name "private/customize.el" user-emacs-directory))

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
(require 'init_main)

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
(defun message-startup-time ()
  (message "Emacs loaded in %dms"
           (/ (- (+ (cl-third after-init-time)
                    (* 1000000 (cl-second after-init-time)))
                 (+ (cl-third before-init-time)
                    (* 1000000 (cl-second before-init-time))))
              1000)))
(add-hook 'after-init-hook 'message-startup-time)
