;;; init_magit.el --- magit -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sakito

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

;; magit
;; https://github.com/magit/magit

;;; Code:
(require 'dash)
(require 'with-editor)
(require 'transient)
(require 'libgit)
(require 'magit)

(setq transient-values-file
      (expand-file-name "var/transient/values.el" user-emacs-directory)
      transient-history-file
      (expand-file-name "var/transient/history.el" user-emacs-directory)
      transient-levels-file
      (expand-file-name "var/transient/levels.el" user-emacs-directory))

(provide 'init_magit)
;;; init_magit.el ends here