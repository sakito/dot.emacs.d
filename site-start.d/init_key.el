;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_key.el --- key settign

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

(if (eq window-system 'x)
    (progn
      (global-set-key "\C-w" 'kill-ring-save)
      (global-set-key "\M-w" 'kill-region)
      ))

;;コマンドキーをMetaキーとして利用
(when mac-p
      (setq mac-command-key-is-meta t)
      (setq mac-command-key-is-meta nil)
      (setq ns-command-modifier (quote meta))
  )

(if (eq window-system 'ns)
    ;(setq ns-alternate-modifier (quote alt))
    (setq ns-command-modifier (quote meta))
  )

;; dndの動作を Emacs22と同じにする
(define-key global-map [ns-drag-file] 'ns-find-file)

;; C-x C-l にて選択範囲を小文字に変換する機能
;; (put 'downcase-region 'disabled nil)
;; C-x C-u にて選択範囲を大文字に変換する機能
;; (put 'upcase-region 'disabled nil)

;; cua-mode に移行
;;(require 'sense-region)
;;(defadvice set-mark-command (around sense-region-set-mark-23 activate)
;;  (if (and (mell-transient-region-active-p)
;;           sense-region-mode)
;;      (copy-face 'region 'sense-region-region-face))
;;  ad-do-it)
;;(sense-region-on)

;; CUA-mode にて矩形選択のみを有効化
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; C-x C-b でバッファリストを開く時に、ウィンドウを分割しない
;(global-set-key "\C-x\C-b" 'buffer-menu)
;; バッファを切り替えるのに C-x C-b で electric-buffer-list を使う
;(global-set-key "\C-x\C-b" 'electric-buffer-list)
;; バッファを切り替えるのに C-x C-b で bs-show を使う anythinに変更した
;(global-set-key (kbd "C-x C-b") 'bs-show)

;(ido-mode 'buffer)
;(setq ido-enable-flex-matching t)
;(setq ido-confirm-unique-completion t)
;(setq ido-default-buffer-method 'samewindow)
;(setq ido-use-filename-at-point t)
;(ido-mode t)
;(ido-everywhere t)
;(set-face-background 'ido-first-match "blue3")
;(set-face-foreground 'ido-subdir "blue3")
;(icomplete-mode 1)

;; shiftと移動でリージョン選択
;(pc-selection-mode)

;; インプッットメソッドの設定
;(setq default-input-method "MacOSX-IM-JP")

;;; インプットメソッド対応パッチにてctrキーをOS側に渡さない設定
;(mac-add-ignore-shortcut '(control))
;; システムに装飾キー渡さない
(when mac-p
  (setq mac-pass-control-to-system nil)
  (setq mac-pass-command-to-system nil)
  (setq mac-pass-option-to-system nil)

  ;; 起動したら US にする
  (add-hook 'after-init-hook 'mac-change-language-to-us)
  ;; minibuffer 内は US にする
  (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
  (mac-translate-from-yen-to-backslash)
  ;; 入力モードを英語に変更
  (setq mac-ts-script-language-on-focus '(0 . 0))
  )

(provide 'init_key)
;;; init_key.el ends here
