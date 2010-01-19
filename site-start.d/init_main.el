;;emacs23.el -- emacs23 init setting elisp file

;; Copyright (C) 2009-2010  sakito

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

;; mac 環境における設定

;;; Code:

;; 環境変数
(require 'init_setenv)
;; フレームサイズ、色、フォントの設定
(require 'init_color)
;; shell、eshell 関連
(require 'init_shell)
;; キー設定
(require 'init_key)
;; 独自関数
(require 'init_function)
;;; SKK
(require 'init_skk)
;; session
(require 'init_session)
;;; Elscreen
(require 'init_elscreen)
;; dired
(require 'init_dired)
;; moccur
(require 'init_moccur)
;; anything
(require 'init_anything)
;; auto-complete
(require 'init_ac)
;; eldoc
(require 'init_eldoc)

;;; プログラミング関連
; womanの設定
;;(require 'init_woman)
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
;; objc
(require 'init_objc)
;; yasnippet
(require 'init_yasnippet)
;; javascript-mode
(require 'init_javascript)

;;; SCM関連
;; Subversion Mode
;; @see http://xsteve.nit.at/prg/vc_svn/
;;(require 'psvn)
;; dvc
;(load-file "~/.emacs.d/lisp/dvc-snapshot/dvc-load.el")
;(setq dvc-prefix-key [(control c) ?h])
; hg-mode
(require 'ahg)


;;; 文書記述関連
;; auto-insert
(require 'init_autoinsert)
;; htmlize
;; @see http://fly.srk.fer.hr/~hniksic/emacs/
(require 'htmlize)
;; howm
(require 'init_howm)
;; muse
;; (require 'init_muse)
;; sdicの設定
;(require 'init_sdic)
;; Dictionary.app 呼びだし
(require 'init_adic)
(define-key global-map "\C-cw" 'ite-dict-func)
;; AUC TeX
;; (require 'init_auctex)
;; htmlhelperの設定
(require 'init_html)
;; css-modeの設定
(require 'init_css)
;;; nxml-mode
;; @http://www.thaiopensource.com/download/
(require 'init_nxml)
;;;; hns-mode
;(require 'init_hnf)
;; bhl mode
;;(require 'init_bhl)
;; rst-mode
(require 'init_rst)

(autoload 'po-mode "po-mode" "Major mode for translators when they edit PO files." t)
(eval-after-load 'po-mode '(load "gb-po-mode"))

;;; ネットワーク関連
;; wl
;; (require 'init_wl)
;; w3m
(require 'init_w3m)
;; navi2ch 常時利用はしないことにしました
;; (require 'init_navi2ch)
;; pukiwiki
;; (require 'init_pukiwiki)
;; irc
;; (require 'init_irc)

;;; private 設定
(require 'init_private)

(provide 'init_main)
;;end emacs23.el
