;;; init_changelog.el --- ChangeLog Setting

;; Copyright (C) 2004  sakito

;; Author: sakito <sakito@sakito.com>

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

;;; Commentary: ChangeLog関連の設定

;; 

;;; Code:

;;; ChangeLogメモ
;; @see http://namazu.org/~satoru/unimag/1/
;;
(defun memo ()
  (interactive)
  (add-change-log-entry 
   nil
   (expand-file-name "~/.emacs.d/var/memo.txt")
   )
  (skk-latin-mode 1)
  )

;(global-set-key "\C-cm" 'memo)

;(defun bookmark ()
;  (interactive)
;  (add-change-log-entry 
;   nil
;   (expand-file-name "~/.emacs.d/var/bookmark.txt")
;   )
;  (skk-latin-mode 1)
;  )

;(global-set-key "\C-cb" 'bookmark)

;;; debian-changelog-mode
;; @see 
;; C-c C-v  新規にエントリを追加する．まずはこれ．
;; C-c C-a  コメントの追加．新たに*が追加される．
;; C-c C-c  登録 and 保存．mail address と time が自動で追加される．
;;
;(autoload 'debian-changelog-mode "debian-changelog-mode"
;  "Major mode for editing Debian-style change logs." t)
;(setq auto-mode-alist
;      (cons '("ChangeLog\\..*" . debian-change-log-mode) auto-mode-alist))

(provide 'init_changelog)
;;; init_changelog.el ends here
