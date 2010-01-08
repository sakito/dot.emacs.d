;;; init_objc.el --- objc

;; Copyright (C) 2009  sakito

;; Author: sakito <sakito@sakito.com>
;; Keywords: languages

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

;; XCode 側設定
;; 環境設定->ファイルタイプ-> text -> sourcecode -> sourcecode.c -> その他 -> Emacs.app
;; 「その他」から選択すること。デフォルトに存在する emacs は Terminal.app が起動してしまう
;; 通常は以下を設定しないとフレームが新規作成される。パッチを当てていると不要
;; (setq ns-pop-up-frames nil)

;; 拡張子が m もしくは mm のファイルは matlab-mode とぶつかる
;; 拡張子が h のファイルをそのまま設定してしまうと C や C++ 開発で困る
;; 以下の設定は実質できない
;(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
;(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))
;(add-to-list 'auto-mode-alist '("\\.h$" . objc-mode))
;; magic-mode-alist を利用してファイル内容を解析してモード設定する
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
;; (setq magic-mode-alist
;;       (append (list
;;                '("\\(.\\|\n\\)*\n@implementation" . objc-mode)
;;                '("\\(.\\|\n\\)*\n@interface" . objc-mode)
;;                '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
;;               magic-mode-alist))

;; init_ac がロードされている前提になっている
(ac-company-define-source ac-source-company-xcode company-xcode)

(setq ac-modes (append ac-modes '(objc-mode)))

;; ヘッダファイルを開くには ヘッダファイルにカーソル併せて C-x C-f すれば良い
;; 上手く動作しないなら (ffap-bindings) を init.el に記述する。普通はデフォルトで on

 (defun xcode:buildandrun ()
  (interactive)
  (do-applescript
   (format
    (concat
     "tell application \"Xcode\" to activate \r"
     "tell application \"System Events\" \r"
     "     tell process \"Xcode\" \r"
     "          key code 36 using {command down} \r"
     "    end tell \r"
     "end tell \r"
     ))))

;; 関連ファイルを開く
(setq cc-other-file-alist
      '(("\\.mm?$" (".h"))
        ("\\.cc$"  (".hh" ".h"))
        ("\\.hh$"  (".cc" ".C"))

        ("\\.c$"   (".h"))
        ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

        ("\\.C$"   (".H"  ".hh" ".h"))
        ("\\.H$"   (".C"  ".CC"))

        ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH$"  (".CC"))

        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hpp" ".hh" ".h"))

        ("\\.hpp$" (".cpp" ".c"))))

;; flymake
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
(defvar flymake-objc-compiler "/Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/gcc")
(defvar flymake-objc-compile-default-options (list "-Wall" "-Wextra" "-fsyntax-only" "-std=c99" "-isysroot" "/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator3.1.2.sdk" "-framework" "CoreFoundation" "-framework" "Foundation" "-framework" "UIKit"))
(defvar flymake-last-position nil)
;; /Developer/Platforms/iPhoneSimulator.platform/Developer/usr/bin/gcc-4.0 -Wall -Wextra -fsyntax-only -std=c99 -isysroot /Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator3.1.2.sdk
(defcustom flymake-objc-compile-options '("-I.")
  "Compile option for objc check."
  :group 'flymake
  :type '(repeat (string)))

(defun flymake-objc-init ()
 (let* ((temp-file (flymake-init-create-temp-buffer-copy
                    'flymake-create-temp-inplace))
        (local-file (file-relative-name
                     temp-file
                     (file-name-directory buffer-file-name))))
   (list flymake-objc-compiler (append flymake-objc-compile-default-options flymake-objc-compile-options (list local-file)))))

(defun flymake-display-err-minibuffer ()
  "Display any errors or warnings for the current line in the minibuffer."
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

(defadvice flymake-goto-next-error (after display-message activate compile)
  "Display the error in the minibuffer."
  (flymake-display-err-minibuffer))

(defadvice flymake-goto-prev-error (after display-message activate compile)
  "Display the error in the minibuffer."
  (flymake-display-err-minibuffer))

(defadvice flymake-mode (before post-command-stuff activate compile)
  "Add functionality to the post command hook so that if the
cursor is sitting on a flymake error the error information is
displayed in the minibuffer."
  (set (make-local-variable 'post-command-hook)
       (cons 'flymake-display-err-minibuffer post-command-hook)))

;; hook の設定
(add-hook 'objc-mode-hook
          (lambda ()
            (define-key objc-mode-map (kbd "\t") 'ac-complete)
            (define-key objc-mode-map (kbd "C-c C-c") 'xcode/build-compile)
            (define-key objc-mode-map (kbd "C-c C-r") 'xcode:buildandrun)
            (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
            (push 'ac-source-company-xcode ac-sources)
            (push 'ac-source-c++-keywords ac-sources)
            (push '("\\.m$" flymake-objc-init) flymake-allowed-file-name-masks)
            (flymake-mode t)
            ;; (which-function-mode t)
          ))

;(push '("\\.m$" flymake-objc-init) flymake-allowed-file-name-masks)
;;(push '("\\.mm$" flymake-objc-init) flymake-allowed-file-name-masks)
;;(push '("\\.h$" flymake-objc-init) flymake-allowed-file-name-masks)


(require 'xcode)


(provide 'init_objc)
;;; init_objc.el ends here
