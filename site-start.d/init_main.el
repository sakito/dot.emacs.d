;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;init_main.el -- init main setting elisp file

;; Copyright (C) 2009-2012 sakito

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

;; 環境における設定
;; 個人的に重要な物を上になるべく配置

;; Emacs 24.4 で ad-advised-definition-p が削除された対応
;; sr-speedbar.el からコピー
(if (not (fboundp 'ad-advised-definition-p))
    (defun ad-advised-definition-p (definition)
      "Return non-nil if DEFINITION was generated from advice information."
      (if (or (ad-lambda-p definition)
              (macrop definition)
              (ad-compiled-p definition))
          (let ((docstring (ad-docstring definition)))
            (and (stringp docstring)
                 (get-text-property 0 'dynamic-docstring-function docstring))))))

;;; Code:

;; 環境変数
(require 'init_setenv)
;; フレームサイズ、色、フォントの設定
(require 'init_color)
;; session
(require 'init_session)
;; recentf
(require 'init_recentf)
;; shell、eshell 関連
(require 'init_shell)
;; Lisp
(require 'init_lisp)
;; キー設定
(require 'init_key)
;; anything
(require 'init_anything)
;; popwin
(require 'init_popwin)
;; 独自関数
(require 'init_function)
;;; SKK
(require 'init_skk)
;; rst-mode
(require 'init_rst)
;; smartchr
(require 'init_smartchr)
;; (require 'init_key-combo)
;; migemo
(require 'init_cmigemo)
;;; Elscreen
(require 'init_elscreen)
;; dired
(require 'init_dired)
;; moccur
(require 'init_moccur)
;; auto-complete
(require 'init_ac)
;; eldoc
(require 'init_eldoc)
;;; プログラミング関連
(require 'init_flymake)
;;; Pythonの設定
(require 'init_python)
;(require 'init_python-mode)
;; vsc and scm
(require 'init_scm)
;; diff
(require 'init_diff)
; woman は mac で動作しない
;;(require 'init_woman)
;;;; mode-info
(require 'init_modeinfo)
;;; Gaucheの設定
(require 'init_gauche)
;;; OCaml
(require 'init_ocaml)
;;;; sql-mode
(require 'init_sql)
;;; mmm-mode
(require 'init_mmm)
;;; smart-compie
(require 'init_smartcompile)
;; c
(require 'init_c)
;; objc
(when mac-p
  (require 'init_objc))
;; yasnippet
(require 'init_yasnippet)
;; javascript-mode
(require 'init_javascript)
;;; 文書記述関連
;; auto-insert
(require 'init_autoinsert)
;; htmlize
;; @see http://fly.srk.fer.hr/~hniksic/emacs/
(require 'htmlize)
;; howm
(require 'init_howm)
;; sdicの設定
;;(require 'init_sdic)
;; Dictionary.app 呼びだし
(require 'init_adic)
;; AUC TeX
;; (require 'init_auctex)
;; htmlhelperの設定
;; (require 'init_html)
;; css-modeの設定
(require 'init_css)
;; nxml-mode
(require 'init_nxml)
;; scala-mode
(require 'init_scala)
;; haskel-mode
(require 'init_haskell)
;; php-mode
(require 'init_php)
;; po-mode
(require 'init_po)
;; w3m
;; (require 'init_w3m)
;; navi2ch
;; (require 'init_navi2ch)
;; irc
;; (require 'init_irc)

(require 'init_speedbar)
(require 'init_org)
;; (require 'init_slime)
(require 'init_calendar)
(require 'init_linum)
(require 'init_go)
;; (require 'init_viewer)
(require 'init_keychord)
;; (require 'init_pyregexp)
(require 'init_expand-region)
(require 'init_erlang)
(require 'init_web-mode)

;;; private 設定
(require 'init_modeline)
(when mac-p
  (require 'init_private))

(provide 'init_main)
;;end init_main.el
