;;; init_session.el --- session                      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  sakito

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 情報永続化関連

;;; Code:

(setopt desktop-save-mode t)
(setopt savehist-mode t)

(setopt savehist-file
        (expand-file-name "var/session/savehist" user-emacs-directory))

;; https://qiita.com/j8takagi/items/64cc011a333345d2d749
(defun my-set-savehist-additional-variables (&optional file)
  (let (histvars othervars)
    (ignore file)
    (setq histvars (apropos-internal "-\\(\\(history\\)\\|\\(ring\\)\\)\\'" 'boundp))
    (setq othervars
          (append othervars
                  (when desktop-save-mode
                    (append
                     desktop-globals-to-save
                     desktop-locals-to-save
                     ))
                  savehist-minibuffer-history-variables
                  savehist-ignored-variables
                  ))
    (dolist (ovar othervars)
      (setq histvars (delete ovar histvars)))
    (setopt savehist-additional-variables histvars)))

(add-hook 'after-load-functions 'my-set-sevehist-additional-variables)


(provide 'init_session)
;;; init_session.el ends here
