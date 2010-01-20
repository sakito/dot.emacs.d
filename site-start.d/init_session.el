;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_session.el --- session.el

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

;; 

;;; Code:

;; 前回のカーソル位置を記憶 session に代替え
;(require 'saveplace)
;(setq-default save-place t)
;(setq save-place-file "~/.emacs.d/var/places.txt")

;; session
(when (require 'session nil t)
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-file-alist 500 t)
                                  (file-name-history 100))
        ;; 保存時でなく閉じた時のカーソル位置を記憶する
        session-undo-check -1)
  ;; ミニバッファ履歴リストの長さ制限を無くす
  (setq history-length t)
  (add-hook 'after-init-hook 'session-initialize))

;; minibuffer history から重複を排除する
(defun minibuffer-delete-duplicate ()
  (let (list)
    (dolist (elt (symbol-value minibuffer-history-variable))
      (unless (member elt list)
        (push elt list)))
    (set minibuffer-history-variable (nreverse list))))
(add-hook 'minibuffer-setup-hook 'minibuffer-delete-duplicate)

;; kill-ring 内の重複を排除する
(defadvice kill-new (before ys:no-kill-new-duplicates activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;; バッファでの同名ファイルに識別子を付与する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; kill-summary
;(autoload 'kill-summary "kill-summary" nil t)
;(global-set-key "\M-y" 'kill-summary)


(provide 'init_session)
;;; init_session.el ends here
