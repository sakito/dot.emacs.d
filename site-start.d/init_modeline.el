;;; init_modeline.el --- mode line

;; Copyright (C) 2012  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: lisp

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

;; mode-line をカスタマイズしてみている

;;; Code:
;; mode-line のフォーマット
(setq-default mode-line-position
              '(:eval
                (list
                 "  ["
                 (propertize "%03l" 'face 'font-lock-type-face)
                 "/"
                 (propertize (format "%d" (count-lines (point-max) (point-min))) 'face 'font-lock-type-face)
                 "("
                 (propertize "%02p" 'face 'font-lock-type-face)
                 ")"
                 ","
                 (propertize "%03c" 'face 'font-lock-type-face)
                 "] "
                 ))
              )

(setq-default mode-line-format
              '(
                (elscreen-display-screen-number ("-" elscreen-e21-mode-line-string))
                "" skk-modeline-input-mode "%e"
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                mode-line-position
                mode-line-modes
                "--"
                (which-func-mode ("" which-func-format ("--" 0 2)))
                (global-mode-string ("" global-mode-string))
                "--"
                ("-%-" 0 3)
              ))

(provide 'init_modeline)
;;; init_modeline.el ends here
