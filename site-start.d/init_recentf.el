;;; init_recentf.el --- recentf

;; Copyright (C) 2011 sakito

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
;; (eval-when-compile (require 'cl))
(require 'recentf)
;; @see http://www.emacswiki.org/cgi-bin/wiki/download/recentf-ext.el
(require 'recentf-ext)
;; recentf ファイルの保存場所を指定。デフォルトはホームの直下
(setq recentf-save-file
      (expand-file-name "var/recentf.cache" user-emacs-directory))

;; @see http://masutaka.net/chalow/2011-10-30-2.html
(defvar my-recentf-list-prev nil)

;; ;; 保存時にメッセージの出力を抑止
;; (defadvice recentf-save-list
;;   (around no-message activate)
;;   "If `recentf-list' and previous recentf-list are equal,
;; do nothing. And suppress the output from `message' and
;; `write-file' to minibuffer."
;;   (unless (equal recentf-list my-recentf-list-prev)
;;     (flet ((message (format-string &rest args)
;;                     (eval `(format ,format-string ,@args)))
;;            (write-file (file &optional confirm)
;;                        (let ((str (buffer-string)))
;;                          (with-temp-file file
;;                            (insert str)))))
;;       ad-do-it
;;       (setq my-recentf-list-prev recentf-list))))

;; ;; クリーンアップ時のメッセージ出力を抑止
;; (defadvice recentf-cleanup
;;   (around no-message activate)
;;   "suppress the output from `message' to minibuffer"
;;   (flet ((message (format-string &rest args)
;;                   (eval `(format ,format-string ,@args))))
;;     ad-do-it))

;; 自動クリーニングを停止 recentf-cleanup
;; tramp や 外部ディスクを利用している場合停止しておかないと面倒な動作になる
(setq recentf-auto-cleanup 'never)

;; anything で便利なので履歴の保存量を多少多めにしておく
(setq recentf-max-saved-items 1000)

;; 保存ファイルの設定に リモートファイル tramp の先等を追加。これを実施すると起動時にパスワード等の確認はされない
(add-to-list 'recentf-keep 'file-remote-p)
(add-to-list 'recentf-keep 'file-readable-p)

;; 除外ファイル
(setq recentf-exclude
      '("\\.elc$"
        "\\.pyc$"
        "\\.cache$"
        ".recentf$"
        ".howm-keys$"
        "^/var/folders/"
        "^/tmp/"))

;; Emacs 終了時の cleanup
;; clean up は必要になったら手動で実施する事
;; (add-hook 'kill-emacs-query-functions 'recentf-cleanup)

;; 一定の未使用時間毎に自動保存
(run-with-idle-timer (* 5 60) t 'recentf-save-list)
(recentf-mode 1)

(provide 'init_recentf)
;;; init_recentf.el ends here
