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
;; 祝日をマークする
(setq calendar-mark-holidays-flag t)
(require 'japanese-holidays)
(setq calendar-holidays
      (append japanese-holidays holiday-local-holidays holiday-other-holidays))
;; 今日をマークする
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)
;; 日曜日を赤字にする
(setq calendar-weekend-marker 'diary)
(add-hook 'today-visible-calendar-hook 'calendar-mark-weekend)
(add-hook 'today-invisible-calendar-hook 'calendar-mark-weekend)


(provide 'init_calendar)
;;; init_calendar.el ends here
