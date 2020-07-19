;;; init_calendar.el --- caledar

;; Copyright (C) 2010  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: calendar

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

;; カレンダーの設定
;; http://www.meadowy.org/meadow/netinstall/browser/branches/3.00/pkginfo/japanese-holidays/japanese-holidays.el?rev=799
;; http://d.hatena.ne.jp/rubikitch/20090216/1234746280

;;; Code:

(require 'calendar)
;; キーの設定
(define-key calendar-mode-map "f" 'calendar-forward-day)
(define-key calendar-mode-map "n" 'calendar-forward-day)
(define-key calendar-mode-map "b" 'calendar-backward-day)

;; week number
(setq calendar-intermonth-text
      '(propertize
        (format "%02dW"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian
                   (list month (- day (1- calendar-week-start-day)) year)))))
        'font-lock-face 'calendar-iso-week-face))

;; https://github.com/emacs-jp/japanese-holidays
(require 'japanese-holidays)
;; 他の国の祝日も表示させたい場合は適当に調整
(setq calendar-holidays
      (append japanese-holidays holiday-local-holidays holiday-other-holidays))

;; 祝日をカレンダーに表示
(setq calendar-mark-holidays-flag t)

;; 土曜日・日曜日を祝日として表示する
;; デフォルトで設定済み
;; (setq japanese-holiday-weekend '(0 6)     ;; 土日を祝日として表示
;;       japanese-holiday-weekend-marker    ;;  土曜日を水色で表示
;;       '(holiday nil nil nil nil nil japanese-holiday-saturday))

;; 今日をマークする
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
(add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)

(provide 'init_calendar)
;;; init_calendar.el ends here
