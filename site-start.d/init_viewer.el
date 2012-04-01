;;; init_viewer.el --- view mode setting

;; Copyright (C) 2012  sakito

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

;; http://www.emacswiki.org/emacs/viewer.el

;;; Code:
(require 'viewer)
(viewer-stay-in-setup)
;; 色の設定
(setq viewer-modeline-color-unwritable "tomato"
      viewer-modeline-color-view "#2aa198")
(viewer-change-modeline-color-setup)

;; 初期から view mode
(viewer-aggressive-setup 'force)
;; 全てのファイルを対象
;;(setq view-mode-by-default-regexp ".*")

(add-hook 'view-mode-hook
          '(lambda()
             (progn
               ;; C-b, ←
               (define-key view-mode-map "h" 'backward-char)
               ;; C-n, ↓
               (define-key view-mode-map "j" 'next-line)
               ;; C-p, ↑
               (define-key view-mode-map "k" 'previous-line)
               ;; C-f, →
               (define-key view-mode-map "l" 'forward-char)
               )))

(provide 'init_viewer)
;;; init_viewer.el ends here
