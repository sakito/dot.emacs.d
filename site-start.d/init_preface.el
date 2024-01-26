;;; init_preface.el --- preface                      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  sakito

;; Author: sakito <sakito@sakito.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 初期ロード用関数定義

;;; Code:

;; 引数を load-path へ追加
;; normal-top-level-add-subdirs-to-load-path はディレクトリ中で
;; [A-Za-z] で開始する物だけ追加する。
;; 追加したくない物は . や _ を先頭に付与しておけばロードしない
;; dolist は Emacs 21 から標準関数なので積極的に利用して良い
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; Emacs Lisp のPathを通す
(add-to-load-path "lisp"
                  ;; 変更したり、自作の Emacs Lisp
                  "local-lisp"
                  ;; private 内には自分専用の物がはいっている
                  ;; 依存は private 内で完結するようにしている
                  "private"
                  ;; 初期設定ファイル
                  "site-start.d")


;; Emacs の種類バージョンを判別するための変数を定義
;; @see http://github.com/elim/dotemacs/blob/master/init.el
(defun x->bool (elt) (not (not elt)))

(defvar emacs27-p (equal emacs-major-version 27))
(defvar emacs28-p (equal emacs-major-version 28))
(defvar emacs29-p (equal emacs-major-version 29))

(defvar darwin-p (eq system-type 'darwin))
(defvar ns-p (featurep 'ns))
(defvar mac-p (and (eq window-system 'mac) (or emacs27-p emacs28-p emacs29-p)))

(defvar linux-p (eq system-type 'gnu/linux))

(defvar nt-p (eq system-type 'windows-nt))
(defvar windows-p (or nt-p))


(provide 'init_preface)
;;; init_preface.el ends here
