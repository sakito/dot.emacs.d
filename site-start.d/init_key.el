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

(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "M-w") 'kill-region)

;; カーソル移動に追随してウィンドウを移動する
;; (defadvice previous-line
;;   (after scroll-up-in-place activate)
;;   (if (> (current-line) 30)
;;       (scroll-down 1)))

;; (defadvice next-line
;;   (after scroll-down-in-place activate)
;;   (if (> (current-line) 30)
;;       (scroll-up 1)))

;; (defun scroll-up-in-place (n)
;;   (interactive "p")
;;   (forward-line (- n)))
;; (defun scroll-down-in-place (n)
;;   (interactive "p")
;;   (forward-line n))
;; (global-set-key "\M-p" 'scroll-up-in-place)
;; (global-set-key "\M-n" 'scroll-down-in-place)


;; C-x C-l にて選択範囲を小文字に変換する機能
;; (put 'downcase-region 'disabled nil)
;; C-x C-u にて選択範囲を大文字に変換する機能
;; (put 'upcase-region 'disabled nil)

;; C-m は 改行とインデントに割り当て(SKK に取られてしまうから)
(global-set-key (kbd "C-m") 'newline-and-indent)

;; cua-mode に移行
;;(require 'sense-region)

;; CUA-mode にて矩形選択のみを有効化
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; window の移動
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(global-set-key (kbd "<C-tab>") 'other-window-or-split)


;; fullscreen を toggle する
(defun toggle-fullscreen ()
  (interactive)
  (if (frame-parameter nil 'fullscreen)
      (set-frame-parameter nil 'fullscreen nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)
    ))
;; (global-set-key (kbd "C-c m") 'toggle-fullscreen)


(when ns-p
  ;; 
  ;; (setq ns-alternate-modifier (quote alt))
;;  (setq ns-command-modifier (quote meta))
  ;; dndの動作を Emacs22と同じにする
  (define-key global-map [ns-drag-file] 'ns-find-file))

(when mac-p
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
  (setq mac-use-input-method-on-system nil)
  ;; 起動したら US にする
  ;; (add-hook 'after-init-hook 'mac-change-language-to-us)
  ;; minibuffer 内は US にする
  (mac-auto-ascii-mode t)
  ;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)

  ;; 入力モードを英語に変更
  ;; (setq mac-ts-script-language-on-focus '(0 . 0))

  ;; smooth scroll を on
  (setq mac-mouse-wheel-smooth-scroll t)

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
  (mac-translate-from-yen-to-backslash)

)

(provide 'init_key)
;;; init_key.el ends here
