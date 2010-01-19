;;; init_shell.el --- shell

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

;; shell 関連

;;; Code:

;;:eshell
;; glob で .* が .. に一致しないようにする
(setq eshell-glob-include-dot-dot nil)

;;; Shellの設定
;; M-x shell
;; @see http://home7.highway.ne.jp/dayan/tips/mac/bash.html
;(setq shell-file-name "/bin/zsh")
(setq shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
;(setq explicit-bash-args '("-login" "-i"))
;(setq shell-command-switch "-c")
;(setq win32-quote-process-args t)

(provide 'init_shell)
;;; init_shell.el ends here
