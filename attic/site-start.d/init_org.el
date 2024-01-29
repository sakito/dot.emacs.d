;;; init_org.el --- org-mode

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

;; org-mode 用の設定

;; yasnippet http://github.com/RickMoynihan/yasnippet-org-mode

;;; Code:

;; org-mode
(require 'org-install)
;; 初期全表示
(setq org-startup-folded "showall")
;; src 内に色を付ける(動作が重くなるのでコメントアウト)
;; (setq org-src-fontify-natively t)
;; src 内でタブを有効にする
;; (setq org-src-tab-acts-natively t)
;; 改行する
(setq org-startup-truncated nil)
(org-remember-insinuate)
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "notes.org"))
(setq org-remember-templates
      '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
        ))

;; org-babel
(require 'ob)

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (setq ac-use-overriding-local-map t)
            (make-variable-frame-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)
            (local-set-key (kbd "<C-tab>") 'other-window-or-split)))

(provide 'init_org)
;;; init_org.el ends here
