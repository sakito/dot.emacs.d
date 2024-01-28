;;; init_helm.el --- helm -*- lexical-binding: t; -*-

;; Copyright (C) 2016 sakito

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

;; helm
;; https://github.com/emacs-helm/helm

;;; Code:
(require 'helm)
(require 'helm-autoloads)
;; (setq ad-redefinition-action 'accept)  ;; redefin が気になる場合は設定
(helm-mode +1)

(setopt helm-M-x-always-save-history t)

;; session の利用を優先するため、emacs終了時にhelmの履歴を保存しない
;; (remove-hook 'kill-emacs-hook 'helm-c-adaptive-save-history)
;; それでも空ファイルが作成さてしまう環境があるので 0 に設定
;; (setq helm-adaptive-history-length 0)

;; mini buffer 起動
(define-key global-map (kbd "C-;") 'helm-mini)
(define-key helm-map (kbd "C-;") 'abort-recursive-edit)

;; C-h で削除を有効に
(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)

;; TAB で補完(あまり良い設定ではない)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; コマンド表示
(define-key global-map (kbd "M-x") 'helm-M-x)

;; バッファ切り替え時の一覧表示
(define-key global-map (kbd "C-x C-b") 'helm-for-files)

;; C-x C-f に helm 割り当て
;; は個人的にものすごくストレスになる事がわかったので設定しないで別キーに設定
;; (ffap-bindings)
(define-key global-map (kbd "C-c C-f") 'find-file-at-point)

;; 拡張パッケージ類設定

;; キーとコマンドの対応検索を有効にする
;; https://github.com/emacs-helm/helm-descbinds
(require 'helm-descbinds)
(helm-descbinds-mode)

;; ;; M-yでkill-ringの内容をpopupする
;; (require 'popup-kill-ring)
;; (setq popup-kill-ring-item-size-max 1000)
;; (global-set-key (kbd "M-y") 'popup-kill-ring)

;; pt
;; https://github.com/syohex/emacs-helm-ag
(require 'helm-ag)
(setq helm-ag-base-command "pt -e --nocolor --nogroup")
(global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
(global-set-key (kbd "M-g s") 'helm-do-ag)
(global-set-key (kbd "C-M-s") 'helm-ag-this-file)

;; flymake
;; https://github.com/tam17aki/helm-flymake
;; (require 'helm-flymake)
(require 'helm-flycheck)


;; コマンド候補
;; http://emacs.stackexchange.com/questions/13539/helm-adding-helm-m-x-to-helm-sources
;; 上記を参考にして、履歴に保存されるように修正
(defvar helm-source-emacs-commands
  (helm-build-sync-source "Emacs commands"
    :candidates (lambda ()
                  (let (commands)
                    (mapatoms (lambda (cmds)
                                (if (commandp cmds)
                                  (push (symbol-name cmds)
                                        commands))))
                    (sort commands 'string-lessp)))
    :coerce #'intern-soft
    :action (lambda (cmd-or-name)
              (command-execute cmd-or-name 'record)
              (setq extended-command-history
                    (cons (helm-stringify cmd-or-name)
                          (delete (helm-stringify cmd-or-name) extended-command-history)))))
  "A simple helm source for Emacs commands.")

(defvar helm-source-emacs-commands-history
  (helm-build-sync-source "Emacs commands history"
    :candidates (lambda ()
                  (let (commands)
                    (dolist (elem extended-command-history)
                      (push (intern elem) commands))
                    commands))
    :coerce #'intern-soft
    :action #'command-execute)
  "Emacs commands history")

(setq helm-mini-default-sources
      '(
        helm-source-flycheck
        ;; helm-source-flymake-warning
        ;; helm-source-flymake-error
        helm-source-buffers-list
        helm-source-file-name-history
        helm-source-recentf
        helm-source-files-in-current-dir
        helm-source-emacs-commands-history
        helm-source-emacs-commands
        helm-source-bookmarks
        ))


(provide 'init_helm)
;;; init_helm.el ends here
