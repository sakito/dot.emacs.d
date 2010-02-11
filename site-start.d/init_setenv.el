;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_setenv.el --- Unix Env Setting

;; Copyright (C) 2004  sakito

;; Author: sakito <sakito@sakito.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary: 環境変数関連の設定

;; 

;;; Code:

;; PATH設定
(add-to-list 'exec-path (expand-file-name "/usr/local/bin"))
(add-to-list 'exec-path (expand-file-name "/usr/bin"))
(add-to-list 'exec-path (expand-file-name "/sw/bin"))
(add-to-list 'exec-path (expand-file-name "~/bin"))
(setenv "PATH" (concat "/usr/local/bin:/sw/bin:~/bin:" (getenv "PATH")))
(setenv "MANPATH" (concat "/usr/bin/man:/usr/local/man:/usr/share/man:/Developer/usr/share/man:/sw/man" (getenv "MANPATH")))

;; JDEEの設定に移動
;;(setenv "JAVA_HOME" "/Library/Java/Home")
;;(setenv "ANT_HOME" "/sw/lib/ant")
;;(setenv "LC_ALL" "en")

(setenv "CVS_RSH" "ssh")
(setenv "DISPLAY" "localhost")
(setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK"))
(setenv "LC_ALL" "ja_JP.UTF-8")

(provide 'init_setenv)
;;; init_setenv.el ends here
