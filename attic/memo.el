;;; memo.el --- memo                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2024  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: 

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

;; 保存memo

;;; Code:


;; ;; 終了時バイトコンパイル
;; (add-hook 'kill-emacs-query-functions
;;           (lambda ()
;;             (if (file-newer-than-file-p
;;                  (expand-file-name "init.el" user-emacs-directory)
;;                  (expand-file-name "init.elc" user-emacs-directory))
;;                 (byte-compile-file
;;                  (expand-file-name "init.el" user-emacs-directory)))
;;             (byte-recompile-directory
;;              (expand-file-name "site-start.d" user-emacs-directory) 0)
;;             ))



(provide 'memo)
;;; memo.el ends here
