;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_modeinfo.el --- mode-info

;; Copyright (C) 2004  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; 

;;; Code:

;; @see http://www.namazu.org/~tsuchiya/elisp/mode-info.html
;(require 'mi-config)
;(define-key global-map "\C-cf" 'mode-info-describe-function)
;(define-key global-map "\C-cv" 'mode-info-describe-variable)
;;(define-key global-map "\M-." 'mode-info-find-tag)
;(require 'mi-fontify)
;(setq mode-info-class-alist
;      '(
;        (elisp  emacs-lisp-mode lisp-interaction-mode)
;        (make   makefile-mode)
;        (octave octave-mode)
;        (gauche scheme-mode scheme-interaction-mode inferior-scheme-mode)
;        ))

;; @see http://d.hatena.ne.jp/mooz/20101207/p1
(add-hook 'Info-mode-hook
          (lambda ()
            ;; 上下カーソル移動
            (local-set-key "j" 'next-line)
            (local-set-key "k" 'previous-line)
            (local-set-key " " 'Info-scroll-up)
            (local-set-key "b" 'Info-scroll-down)
            (local-set-key "g" 'beginning-of-buffer)
            (local-set-key "G" 'end-of-buffer)
            ;; 上下スクロール (カーソル固定)
            (local-set-key "J" (lambda () (interactive) (scroll-up 1)))
            (local-set-key "K" (lambda () (interactive) (scroll-down 1)))
            ;; 左右カーソル移動
            (local-set-key "l" 'forward-word)
            (local-set-key "h" 'backward-word)
            ;; 戻る・進む
            (local-set-key "B" 'Info-history-back)
            (local-set-key "F" 'Info-history-forward)
            ;; リンクを開く
            (local-set-key "o" 'Info-follow-nearest-node)
            ;; 次・前のリンクへカーソル移動
            (local-set-key "n" 'Info-next-reference)
            (local-set-key "p" 'Info-prev-reference)
            ))

(provide 'init_modeinfo)
;;; init_modeinfo.el ends here
