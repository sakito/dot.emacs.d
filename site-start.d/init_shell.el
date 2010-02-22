;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_shell.el --- shell

;; Copyright (C) 2010  sakito

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

;; shell 関連

;;; Code:

;;:eshell
;; glob で .* が .. に一致しないようにする
(setq eshell-glob-include-dot-dot nil)

;;; Shellの設定
;; M-x anshi-term、term
(setq shell-file-name (executable-find "zsh"))
(setq explicit-shell-file-name (executable-find "zsh"))
;(setq shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
;(setq explicit-bash-args '("-login" "-i"))
;(setq shell-command-switch "-c")
;(setq win32-quote-process-args t)
(setq system-uses-terminfo t)

(require 'term)

(defun term-send-escape  () (interactive) (term-send-raw-string (kbd "ESC")))
(eval-after-load 'term
  (progn
    ;; C-c ESC でエスケープを送信
    '(define-key term-raw-escape-map (kbd "ESC") 'term-send-escape)
    ;; キーの上書きを停止
    '(mapcar
      (lambda (key) (define-key term-raw-map key nil))
      (list
       (kbd "M-:")
       (kbd "M-w")
       (kbd "M->")
       (kbd "M-<")

       (kbd "C-\\")
       (kbd "C-a")
       (kbd "C-e")
       (kbd "C-f")
       (kbd "C-h")
       (kbd "C-j")
       (kbd "C-k")
       (kbd "C-l")
       (kbd "C-u")
       (kbd "C-y")))))

(add-hook 'term-mode-hook
          '(lambda ()
             ;; Emacs の標準的キー割り当てにする
             (define-key term-raw-map (kbd "C-p") 'previous-line)
             (define-key term-raw-map (kbd "C-n") 'next-line)
             ;; (define-key term-raw-map (kbd "C-y") nil)
             (define-key term-raw-map (kbd "M-x") 'execute-extended-command)
             ;; 削除
             (define-key term-raw-map (kbd "C-h") 'term-send-backspace)
             ;; elscreen de C-z を利用するので有効にする
             (define-key term-raw-map (kbd "C-z")
               (lookup-key (current-global-map) (kbd "C-z")))))


;;; shell-mode でエスケープを綺麗に表示
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; shell-pop の設定
;; @see http://www.emacswiki.org/emacs-en/ShellPop
(require 'shell-pop)
(shell-pop-set-internal-mode "term")
(shell-pop-set-window-height 40)

(shell-pop-set-internal-mode-shell (executable-find "zsh"))
(global-set-key [f8] 'shell-pop)

(provide 'init_shell)
;;; init_shell.el ends here
