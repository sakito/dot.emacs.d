;;; init_anything.el --- anything.el setting

;; Copyright (C) 2009-2010  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

(global-set-key (kbd "C-;") 'anything)
(setq anything-sources
      '(
        anything-c-source-mac-spotlight
        anything-c-source-buffers+
;;        anything-c-source-buffer-not-found
        anything-c-source-file-name-history
        anything-c-source-kill-ring
        anything-c-source-bookmarks
;;        anything-c-source-info-pages
;;        anything-c-source-info-elisp
;;        anything-c-source-man-pages
;;        anything-c-source-locate
;;        anything-c-source-emacs-commands
;;        anything-c-source-emacs-functions
        ))

(global-set-key (kbd "C-c i") 'anything-help-at-point)
;; 検索の対象を変更した物を作成
(defun anything-help-at-point ()
  "Preconfigured `anything' for searching help at point."
  (interactive)
  (anything '(anything-c-source-info-elisp
              anything-c-source-info-cl
              anything-c-source-man-pages
              anything-c-source-info-pages
              anything-c-source-emacs-commands
              anything-c-source-emacs-functions
              )
            (thing-at-point 'symbol) nil nil nil "*anything help*"))

;; このままだと woman が動作して man が見れないので man が動作するように変更する
;; (defvar anything-c-source-man-pages+
;;   `((name . "Manual Pages")
;;     (candidates . (lambda ()
;;                     (if anything-c-man-pages
;;                         anything-c-man-pages
;;                       ;; XEmacs doesn't have a woman :)
;;                       (setq anything-c-man-pages
;;                             (ignore-errors
;;                               (require 'man)
;;                               (woman-file-name "")
;;                               (sort (mapcar 'car woman-topic-all-completions)
;;                                     'string-lessp))))))
;;     (action  ("Show with man" . man))
;;     (requires-pattern . 2)))
;; (anything 'anything-c-source-man-pages+)

;; メンテナンスされてないので利用しちゃいけない
;; (iswitchb-mode)
;; (anything-iswitchb-setup)

;;; Spotlight (MacOS X desktop search)
(defvar anything-c-source-mac-spotlight-home
  '((name . "mdfindhome")
    (candidates . (lambda ()
                    (start-process "mdfind-process" nil "mdfind" "-onlyin" "~/" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")
;; (anything 'anything-c-source-mac-spotlight-home)


;; 最近のファイル等を anything する
;; see http://www.emacswiki.org/cgi-bin/wiki/download/recentf-ext.el
;; 自動クリーニングを停止 recentf-cleanup
(setq recentf-auto-cleanup 'never)
;; anything で便利なので履歴の保存量を多少多めにしておく
(setq recentf-max-saved-items 1000)

;; 保存ファイルのの設定に リモートファイル tramp の先等を追加。これを実施すると起動時にパスワード等の確認はされない
(when (boundp 'recentf-keep) (add-to-list 'recentf-keep 'file-remote-p))
;; 除外ファイル
(setq recentf-exclude
      '("\\.elc$"
        "\\.pyc$"
        ".recentf$"
        ".howm-keys$"
        "^/var/folders/"
        "^/tmp/"))
(add-hook 'kill-emacs-hook 'recentf-cleanup)
;; recentf ファイルの保存場所を指定。デフォルトはホームの直下
;; (setq recentf-save-file "~/.emacs.d/var/recentf")
(require 'recentf-ext)
(global-set-key (kbd "C-x C-b") 'anything-for-files)

;; M-yでkill-ringの内容をanythingする
(global-set-key (kbd "M-y") 'anything-show-kill-ring)

;; C-x C-b のバッファリストをanythingする
;(global-set-key (kbd "C-x C-b") 'anything-for-buffers)  これだと色つかないので以下にした
;(global-set-key (kbd "C-x C-b") (lambda () (interactive) (anything 'anything-c-source-buffers+)))
;; current-buffer も末尾に表示する
(setq anything-allow-skipping-current-buffer nil)

;;split-root http://nschum.de/src/emacs/split-root/
;; anything した時のウィンドウを常に下部に開く。高さは比率にて自動算出
(require 'split-root)
;; 比率
(defvar anything-compilation-window-height-percent 30.0)
(defun anything-compilation-window-root (buf)
  (setq anything-compilation-window
        (split-root-window (truncate (* (window-height)
                                        (/ anything-compilation-window-height-percent
                                           100.0)))))
  (set-window-buffer anything-compilation-window buf))
(setq anything-display-function 'anything-compilation-window-root)


;;; anything-c-moccurの設定
;; see http://d.hatena.ne.jp/IMAKADO/20080724/1216882563
(require 'anything-c-moccur)
(setq
 ;;`anything-idle-delay'
 anything-c-moccur-anything-idle-delay 0.2
 ;; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
 anything-c-moccur-higligt-info-line-flag t
 ;; 現在選択中の候補の位置を他のwindowに表示する
 anything-c-moccur-enable-auto-look-flag t
 ;; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする
 anything-c-moccur-enable-initial-pattern t)

;; バッファ内検索
(global-set-key (kbd "M-o") 'anything-c-moccur-occur-by-moccur)
;;ディレクトリ
;(global-set-key (kbd "C-M-o") 'anything-c-moccur-dmoccur)
(global-set-key (kbd "C-M-o") 'moccur-grep)
;; dired
(add-hook 'dired-mode-hook
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;; session を利用するため anything-c-adaptive-save-history を作成しない
(remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)
(ad-disable-advice 'anything-exit-minibuffer 'before 'anything-c-adaptive-exit-minibuffer)
(ad-disable-advice 'anything-select-action 'before 'anything-c-adaptive-select-action)
(setq anything-c-adaptive-history-length 0)

;; キー設定
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-v") 'anything-next-source)
(define-key anything-map (kbd "M-v") 'anything-previous-source)

(provide 'init_anything)
;;; init_anything.el ends here
