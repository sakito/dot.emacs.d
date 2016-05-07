;;; init_popwin.el --- popwin

;; Copyright (C) 2011-2012 sakito

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

;; popwin 設定ファイル


;;; Code:
(require 'popwin)
(defvar popwin:special-display-config-backup popwin:special-display-config)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-height 0.4)
(setq anything-samewindow nil)
(setq popwin:special-display-config
      (append '(
                ("\\*anything" :regexp t :height 20)
                ("\\*helm" :regexp t :height 20)
                ;;("*anything for files*" :height 20)
                ;;("*anything help*" :height 20)
                ("*Compile-Log*" :height 10 :noselect t)
                (dired-mode :position top)
                ;; ("*terminal<1>*")
                ;; ("*Remember*" :stick t)
                ;; ("*Org Agenda*")
                ("*Backtrace*")
                ("*sdic*" :noselect)
                ("*aHg diff*" :position top :height 50)
                ("*aHg log*" :position left)
                ("\\*hg command" :regexp t :noselect)
                )
              popwin:special-display-config))
;; (define-key global-map (kbd "C-x p") 'popwin:display-last-buffer)
(define-key dired-mode-map "o" #'(lambda ()
                                   (interactive)
                                   (popwin:find-file (dired-get-file-for-visit))))
;; (key-chord-define-global "mn" 'popwin:messages)

;; http://cx4a.blogspot.com/2011/12/popwineldirexel.html
(require 'direx)
;; direx:direx-modeのバッファをウィンドウ左辺に幅25でポップアップ
;; :dedicatedにtを指定することで、direxウィンドウ内でのバッファの切り替えが
;; ポップアップ前のウィンドウに移譲される
(push '(direx:direx-mode :position right :width 30)
      popwin:special-display-config)
(global-set-key (kbd "C-c C-s") 'direx:jump-to-directory-other-window)

(provide 'init_popwin)
;;; init_popwin.el ends here
