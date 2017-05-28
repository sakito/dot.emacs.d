;;; init_shackle.el --- shackle  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  sakito

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

;; shackle 設定ファイル

;;; Code:
(require 'shackle)

(setq helm-display-function 'pop-to-buffer)
;; default nil
(setq shackle-select-reused-windows nil)
;;default below
(setq shackle-default-alignment 'below)
;; default 0.5
(setq shackle-default-size 0.4)

(setq shackle-rules
      '((compilation-mode :select nil)
        ("*Completions*" :size 0.3  :align t)
        ("*Messages*" :select nil :inhibit-window-quit t :other t)
        ("*Compile-Log*" :size 10 :select nil)

        ("*Help*" :select t :inhibit-window-quit t :other t)
        ("*info*" :select t :inhibit-window-quit t :same t)
        ("\\*[Wo]*Man.*\\*" :regexp t :select t :inhibit-window-quit t :other t)

        ("\\`\\*helm.*?\\*\\'" :regexp t :size 0.3 :align t)

        ("*eshell*" :select t :other t)
        ("*Shell Command Output*" :select nil)
        ("\\*Async Shell.*\\*" :regexp t :ignore t)
        ("\\*poporg.*\\*" :regexp t :select t :other t)

        ("*Calendar*" :select t :size 0.3 :align below)

        ("*aHg diff*" :sise 50 :align above :select t)
        ("*aHg log*" :align left)
        ("\\*hg command" :regexp t :select nil)

        (magit-status-mode :select t :inhibit-window-quit t :same t)
        (magit-log-mode :select t :inhibit-window-quit t :same t)
        ))

(shackle-mode 1)


(provide 'init_shackle)
;;; init_shackle.el ends here

