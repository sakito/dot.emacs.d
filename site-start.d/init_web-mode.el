;;; init_web-mode.el --- web-mode

;; Copyright (C) 2013 sakito

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

(require 'web-mode)

(add-to-list 'auto-mode-alist
             '("\\.\\(html\\|htm\\)\\'" . web-mode))

(eval-after-load "web-mode"
  '(progn
     (add-hook 'web-mode-hook
               (lambda()
                 ;; web-modeの設定
                 ;; html
                 (setq web-mode-markup-indent-offset 2)
                 ;; css
                 (setq web-mode-css-indent-offset 2)
                 ;; js, php, etc..
                 (setq web-mode-code-indent-offset 2)
                 (setq web-mode-comment-style 2)
                 ;; キーの設定
                 (define-key web-mode-map  (kbd "C-;") nil)
                 (define-key web-mode-map  (kbd "C-c C-;") 'web-mode-comment-or-uncomment)
                 ))
  ))




(provide 'init_web-mode)
;;; init_web-mode.el ends here
