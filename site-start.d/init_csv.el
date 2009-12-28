;;; init_csv.el --- csv

;; Copyright (C) 2008  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:

;;; CVSインターフェース
;; pcl-cvs
;;
;(add-hook 'cvs-mode-hook
;          (lambda ()
;            (collection-set-goal-column cvs-buffer-name 38)))

;; log はどのコードで書くか決めて、固定にしておかないと危険です 
;; (euc-japan でも sjis でも良いんけど)
(modify-coding-system-alist 'process "cvs" '(undecided . euc-japan))

;(eval-after-load "pcvs"
;  '(progn
;     ;; workaround a strange bug that only happens on my machine
;     (load-file (locate-library "pcvs-util.el"))
;     (setq cvs-update-flags nil)
;     (cvs-flags-define cvs-update-flags (cvs-defaults '("-P")))))



(provide 'init_csv)
;;; init_csv.el ends here
