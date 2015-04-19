;;; init_flymake.el --- flymake

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

;; flymake

;;; Code:

;; flymakeを設定
(require 'flymake)
;; (require 'flymake_fringe)

;; flymake 起動頻度 初期値:0.5
(setq-default flymake-no-changes-timeout '3)

;; make 用
(defun flymake-get-make-cmdline (source base-dir)
  (list "make"
        (list "-s" "-C"
              base-dir
              (concat "CHK_SOURCES=" source)
              "SYNTAX_CHECK_MODE=1")))

;; flymake の on off を toggle する(この関数は本来の利用ならば不要です)
(defun flymake-mode-toggle ()
  (if flymake-mode
    (flymake-mode-off)
    (flymake-mode-on)))

;; flymakeのエラー表示をミニバッファに表示
(defun flymake-display-err-minibuffer ()
  "Display any errors or warnings for the current line in the minibuffer."
  (interactive)
  (let* ((line-no (line-number-at-pos))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; 次のエラーに飛ぶ
(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the minibuffer."
  (flymake-display-err-minibuffer))

;; 前のエラーに飛ぶ
(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the minibuffer."
  (flymake-display-err-minibuffer))

;; 自動でエラー表示
(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer."
  (set (make-local-variable 'post-command-hook)
       (add-hook 'post-command-hook 'flymake-display-err-minibuffer)))

;;(define-key global-map (kbd "C-c e") 'flymake-display-err-minibuffer)

(provide 'init_flymake)
;;; init_flymake.el ends here
