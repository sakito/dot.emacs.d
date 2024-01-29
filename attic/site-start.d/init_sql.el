;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_sql.el --- sql mode setting file

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

(autoload 'master-mode "master" "Master mode minor mode." t)

;; SQL mode 起動で sql 関連 lisp をロード
(eval-after-load "sql"
  '(progn
     ;; http://www.emacswiki.org/emacs/sql-indent.el
     (load-library "sql-indent")
     ;; http://www.emacswiki.org/emacs/sql-complete.el
     (load-library "sql-complete")
     ;; http://www.emacswiki.org/emacs/sql-transform.el
     (load-library "sql-transform")
     ))

;; 接続プログラムの名称
(setq sql-postgres-program "psql")
(setq sql-mysql-program "mysql")
(setq sql-sqlite-program "sqlite3")

;; データベース接続初期設定
(setq sql-database "test")
(setq sql-user "testuser")
(setq sql-password "")

;; SQL モード設定
(add-hook 'sql-mode-hook
          (function (lambda ()
                      (setq sql-indent-offset 2)
                      (setq sql-indent-maybe-tab nil)
                      (local-set-key "\C-cu" 'sql-to-update) ; sql-transform
                      ;; SQLi の自動ポップアップ
                      (setq sql-pop-to-buffer-after-send-region t)
                      ;; master モードを有効にし、SQLi をスレーブバッファにする
                      (master-mode t)
                      (master-set-slave sql-buffer)
                      )))

(add-hook 'sql-set-sqli-hook
          (function (lambda ()
                      (master-set-slave sql-buffer))))

;; interactive-mode の設定
(add-hook 'sql-interactive-mode-hook
          (function (lambda ()
                      ;; 「;」をタイプしたら SQL 文を実行
                      (setq sql-electric-stuff 'semicolon)
                      ;; comint 関係の設定
                      (setq comint-buffer-maximum-size 500)
                      (setq comint-input-autoexpand t)
                      (setq comint-output-filter-functions
                            'comint-truncate-buffer)
                      )))

(provide 'init_sql)
;;; init_sql.el ends here
