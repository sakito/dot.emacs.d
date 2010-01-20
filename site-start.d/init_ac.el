;;; -*- mode: emacs-lisp; coding: utf-8-emacs-unix; indent-tabs-mode: nil -*-

;;; init_ac.el --- ac

;; Copyright (C) 2009-2010  sakito

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

;; 

;;; Code:

;(require 'ac-mode)
;; 常にac-modeをONにする
;(add-hook 'find-file-hooks 'ac-mode-without-exception)

;; @see http://github.com/m2ym/auto-complete
;; @see http://www.emacswiki.org/emacs/AutoComplete
(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-yasnippet)

;; @see http://nschum.de/src/emacs/company-mode/
;; @see http://github.com/buzztaiki/auto-complete/blob/master/ac-company.el
(require 'ac-company)
;; company
(ac-company-define-source ac-source-company-elisp company-elisp)
(ac-company-define-source ac-source-company-css company-css)
(ac-company-define-source ac-source-company-eclim company-eclim)
(ac-company-define-source ac-source-company-nxml company-nxml)

(add-hook 'emacs-lisp-mode-hook
          '(lambda()
             (push 'ac-source-company-elisp ac-sources)))
(add-hook 'css-mode-hook
          '(lambda()
             (push 'ac-source-company-css ac-sources)))

;; 対象の全てで補完を有効にする
(global-auto-complete-mode t)

;; 補完対象のモードを追加
(setq ac-modes (append ac-modes '(rst-mode)))
(setq ac-modes (append ac-modes '(css-mode)))

;; デフォルトの補完候補
(set-default 'ac-sources '(ac-source-yasnippet ac-source-abbrev ac-source-words-in-same-mode-buffers))

;; キー設定
;(define-key ac-complete-mode-map "\t" 'ac-complete)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)
(define-key ac-completing-map (kbd "M-/") 'ac-stop)

;; 自動で起動するのを停止
(setq ac-auto-start nil)
;; 数字を指定すると ac が起動する文字数になる
;(setq ac-auto-start 2)
;; 起動キーの設定
(ac-set-trigger-key "TAB")
;; 候補の最大件数 デフォルトは 10件
(setq ac-candidate-max 20)

(provide 'init_ac)
;;; init_ac.el ends here
