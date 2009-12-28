;;; init_hnf.el --- Hyper Nikki System File Mode setting

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

;; @see http://www.nijino.com/ari/programs/hnf-mode/
;; M-x hnsにより新規日記記述開始
(autoload 'hnf "hnf-mode" nil t)
(autoload 'hnf-mode "hnf-mode" nil t)
(setq auto-mode-alist (cons '("\\.hnf$" . hnf-mode) auto-mode-alist))
(modify-coding-system-alist 'file "\\.hnf$" '(undecided . euc-jp))
(setq hnf-diary-year-directory-flag t)
;; Calendarと連携
(autoload 'hnf-mark-diary-entries "hnf-mode" nil t)
(add-hook 'calendar-load-hook
          '(lambda ()
             (define-key calendar-mode-map "iD" 'hnf-insert-diary-entry)
             ))
(add-hook 'today-visible-calendar-hook 'hnf-mark-diary-entries)
(add-hook 'today-invisible-calendar-hook 'hnf-mark-diary-entries)
(setq hnf-diary-url "http://www.sakito.com/diary/")
(setq hnf-cat '(
                ("Mac") ("Emacs") ("Book") ("Python") ("Java") ("Info") ("PythonFaq")
                ("h独り言") ("2ちゃんねる") ("Memo") ("Thinking") ("s宗教") ("Politics")
                ("Business") ("Education")
                ))

(setq hnf-initial-function
      (function
       (lambda ()
         (insert "OK\n\n"))))

(add-hook 'hnf-mode-load-hook
          '(lambda ()
             (progn
               (define-key hnf-mode-map "\C-c\C-m" '(lambda () (interactive) (insert "~\n\n")))
             )))



(provide 'init_hnf)
;;; init_hnf.el ends here
