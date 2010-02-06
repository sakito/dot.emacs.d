;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_sysns.el --- ns

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

;; Emacs 23 nextstep port 用の設定

;;; Code:

;; 等幅の動作確認を実施してある物
(create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
(set-fontset-font "fontset-menlokakugo"
                  'unicode
                  (font-spec :family "Hiragino Kaku Gothic ProN" :size 16)
                  nil
                  'append)

(create-fontset-from-ascii-font "September-16:weight=normal:slant=normal" nil "septemberipagothic")
(set-fontset-font "fontset-septemberipagothic"
                  'unicode
;;                  'japanese-jisx0213.2004-1
                  (font-spec :family "IPAGothic")
                  nil
                  'append)
(set-fontset-font "fontset-septemberipagothic"
                  '( #x3000 .  #x30ff)
                  (font-spec :family "September")
                  nil
                  'prepend)
(set-fontset-font "fontset-septemberipagothic"
                  '( #xff00 .  #xffef)
                  (font-spec :family "September")
                  nil
                  'prepend)

;;(add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))  ;; 実際に設定する場合
(add-to-list 'default-frame-alist '(font . "fontset-septemberipagothic"))  ;; 実際に設定する場合

(set-frame-parameter (selected-frame) 'alpha '(85 50))
;; (setq ns-alternate-modifier (quote alt))
(setq ns-command-modifier (quote meta))

;; dndの動作を Emacs22と同じにする
(define-key global-map [ns-drag-file] 'ns-find-file)

(setq ns-pop-up-frames nil)

;; objc の設定
(require 'init_ac)
(require 'flymake)
(require 'init_objc)

(provide 'init_sysns)
;;; init_sysns.el ends here
