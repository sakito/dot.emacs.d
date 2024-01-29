;;; init_key-combo.el --- key-combo

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

;; flex-autopair と key-combo の設定
;; https://github.com/uk-ar/flex-autopair
;; https://github.com/uk-ar/key-combo

;;; Code:
(require 'flex-autopair)
(flex-autopair-mode 1)

(defvar flex-autopair-python-conditions
  '(((and (eq last-command-event ?\()) . pair)
    ((and (eq last-command-event ?\[)) . pair)
    ((and (eq last-command-event ?{)) . pair)
    ((and (eq last-command-event ?')) . pair)
    )
  )

(defun flex-autopair-python-mode-setup ()
  (setq flex-autopair-default-conditions flex-autopair-python-conditions)
  (flex-autopair-reload-conditions)
  )

(add-hook 'python-mode-hook
          'flex-autopair-python-mode-setup)

(require 'key-combo)
(key-combo-load-default)
(defvar key-combo-python-default
  '((","  . ", ")
    ("#"  . (key-combo-execute-orignal "# " "### "))
    ("="  . (" = " " == "))
    ("=>" . " => ")
    ("+" . (" + " " += 1"))
    ("+=" . " += ")
    ("-" . (" - " " -= 1"))
    ("-=" . " -= ")
    ("->" . " -> ")
    (">" . (key-combo-execute-orignal ">>> " " => " " => '`!!''" " => \"`!!'\""))
    (">=" . " >= ")
    ("%"  . " % ")
    ("%="  . " %= ")
    ("!" . (key-combo-execute-orignal))
    ("!="  . " != " )
    ("*"  . " * " )
    ("*="  . " *= " )
    ("<" . (key-combo-execute-orignal))
    ("<=" . " <= ")
    ("\"" . (key-combo-execute-orignal "\"\"\"\n\"\"\""))
    ("'" . (key-combo-execute-orignal))
    ("''" . "'''\n'''")
    ))

(key-combo-define-hook 'python-mode-hook
                       'key-combo-python-load-default
                       key-combo-python-default)


(provide 'init_key-combo)
;;; init_key-combo.el ends here
