;;; init_slime.el --- slime

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

;; http://common-lisp.net/project/slime/
;; http://trac.clozure.com/ccl

;; 現在 ccl でしかほとんど動作確認してません
;; Clojure の設定は含まれません

;;; Code:

;; 文字コードの設定
(setq slime-net-coding-system 'utf-8-unix)

;; 特定の実装のみを利用する場合
;;(setq inferior-lisp-program (expand-file-name "~/opt/ccl/scripts/ccl64 -K utf-8"))

;; 複数実装を切り変える場合は以下
(setq slime-lisp-implementations
      `(
        (ccl (,(expand-file-name "~/opt/ccl/scripts/ccl64") "-K"  "utf-8"))
        ;; (ccl ("/opt/local/bin/ccl"))
        (abcl ("/opt/local/bin/abcl"))
        (clisp ("/opt/local/bin/clisp"))
        (ecl ("/usr/local/bin/ecl"))
        (gcl ("/usr/local/bin/gcl"))
        (sbcl ("/opt/local/bin/sbcl"))
        ))
(setq slime-default-lisp 'ccl)

(require 'slime)
(slime-setup)
(require 'slime-autoloads)

(add-to-list 'auto-mode-alist '("\\.cl$" . common-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.asd$" . common-lisp-mode))

(setq slime-truncate-lines nil)
(setq slime-enable-evaluate-in-emacs t)

;; http://github.com/purcell/ac-slime/blob/master/ac-slime.el
(setq ac-modes (append ac-modes '(lisp-mode)))
(setq ac-modes (append ac-modes '(common-lisp-mode)))
(setq ac-modes (append ac-modes '(slime-repl-mode)))

(require 'ac-slime)
(defun skt:slime-hook ()
  (skt:start-slime)
  'set-up-slime-ac
  (local-set-key (kbd "C-c C-z") 'slime-horizontally)
  ;; キーはなんとなく
  (local-set-key (kbd "C-c C-o") 'skt:slime-repl-send-region)
  ;; 既存の C-c C-d 系のキー
  ;; a slime-apropos
  ;; z slime-apropos-all
  ;; p slime-apropos-package
  ;; d slime-describe-symbol
  ;; f slime-describe-function
  ;; h slime-documentation-lookup
  ;; ~ common-lisp-hyperspec-format
  ;; # common-lisp-hyperspec-lookup-reader-macro
  ;; 一応上書きしないようにしている
  (local-set-key (kbd "C-c C-d i") 'anything-hyperspec-and-cltl2)
  (local-set-key (kbd "C-c C-d m") 'amop-lookup)
  )

(add-hook 'slime-lisp-mode-hook 'skt:slime-hook)
(add-hook 'slime-mode-hook 'skt:slime-hook)
(add-hook 'slime-repl-mode-hook 'skt:slime-hook)

(eval-after-load "slime"
  '(progn
     (slime-setup '(
                    slime-fancy
                    slime-indentation
                    slime-references
                    slime-tramp
                    slime-asdf
                    slime-banner
                    anything-slime
                    ))
     ;; setup の引数だと動作しない環境があるので設定
     (require 'slime-repl)(slime-repl-init)
     (require 'slime-fancy)
     (require 'slime-indentation)
     (require 'slime-references)
     (require 'slime-tramp)
     (require 'slime-asdf)
     (require 'slime-banner)
     (require 'anything-slime)(anything-slime-init)
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
     ;; (setq lisp-indent-function 'cl-indent:function)
     ))


;; HyperSpec
;; sudo port install lisp-hyperspec
(setq common-lisp-hyperspec-root "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/")
(setq common-lisp-hyperspec-symbol-table (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))

;; HyperSpec を w3m で見る
(defadvice common-lisp-hyperspec
  (around hyperspec-lookup-w3m () activate)
  (let* ((window-configuration (current-window-configuration))
         (browse-url-browser-function
          `(lambda (url new-window)
             (w3m-browse-url url nil)
             (let ((hs-map (copy-keymap w3m-mode-map)))
               (define-key hs-map (kbd "q")
                 (lambda ()
                   (interactive)
                   (kill-buffer nil)
                   (set-window-configuration ,window-configuration)))
               (use-local-map hs-map)))))
    ad-do-it))


;; cltl2
;; @see http://www.cs.cmu.edu/Groups/AI/html/cltl/cltl2.html
;; @see http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/lisp/doc/cltl/cltl_ht.tgz
;; @see http://www.uuhaus.de/software/cltl2.el
;; @see http://d.hatena.ne.jp/kitokitoki/20100912/p1
(require 'cltl2)
(setq cltl2-root-url (concat  "file://" (expand-file-name user-emacs-directory) "share/cltl/"))

;; cltl2 を w3m で見る
(defadvice cltl2-lookup (around cltl2-lookup-by-w3m () activate)
  (let* ((window-configuration (current-window-configuration))
         (browse-url-browser-function
          `(lambda (url new-window)
             (w3m-browse-url url nil)
             (let ((cltl2-map (copy-keymap w3m-mode-map)))
               (define-key cltl2-map (kbd "q")
                 (lambda ()
                   (interactive)
                   (kill-buffer nil)
                   (set-window-configuration ,window-configuration)))
               (use-local-map cltl2-map)))))
    ad-do-it))

;; Hyperspec と cltl2 を anything で引けるようにする
(eval-after-load "anything"
  '(progn
     (setq anything-c-source-hyperspec
           `((name . "Lookup Hyperspec")
             (candidates . (lambda ()
                             (let ((symbols nil))
                               (mapatoms #'(lambda (sym) (push (symbol-name sym) symbols))
                                         common-lisp-hyperspec-symbols)
                               symbols)))
             (action . (("Show Hyperspec" . hyperspec-lookup)))))

     (setq anything-c-source-cltl2
           `((name . "Lookup CLTL2")
             (candidates . (lambda ()
                             (let ((symbols nil))
                               (mapatoms #'(lambda (sym) (push (symbol-name sym) symbols))
                                         cltl2-symbols)
                               symbols)))
             (action . (("Show CLTL2" . cltl2-lookup)))))

     (defun anything-hyperspec-and-cltl2 ()
       (interactive)
       (anything (list anything-c-source-hyperspec anything-c-source-cltl2) (thing-at-point 'symbol)))))

;; The Art of the Metaobject Protocol(AMOP) を引く
;; @see http://gist.github.com/25243
(defun amop-lookup ()
  (interactive)
  (browse-url
   (format "http://www.lisp.org/mop/dictionary.html#%s"
           (let* ((name (thing-at-point 'symbol))
                  (pos (position ?: name)))
             (if pos
                 (subseq name (1+ pos))
               name)))))

;; Anaphoric Macro aif の定義
;; @see http://www.komaba.utmc.or.jp/~flatline/onlispjhtml/anaphoricMacros.html
(defmacro aif (p true-clause &rest false-clause)
  (declare (indent 2))
  `(let ((it ,p))
     (if it ,true-clause ,@false-clause)))


;; 縦分割して repl を表示
;; @see http://gist.github.com/608169
(defun slime-horizontally ()
  (interactive)
  (let ((buf (window-buffer)))
    (aif (get-buffer "*slime-repl ccl*")
         (my-pop-to-buffer-horizontally it)
         (progn
           (split-window-horizontally)
           (other-window 1)
           (slime)))
    (select-window (car (get-buffer-window-list buf)))))

(defun my-pop-to-buffer-horizontally (buffer-or-name)
  (let ((split-width-threshold 1))
    (pop-to-buffer buffer-or-name)))

;; region を repl で実行
(defun skt:slime-repl-send-region (start end)
  "Send region to slime-repl."
  (interactive "r")
  (let ((buf-name (buffer-name (current-buffer)))
        (ccl-buf (get-buffer "*slime-repl ccl*")))
    (cond (ccl-buf
           (copy-region-as-kill start end)
           (switch-to-buffer-other-window ccl-buf)
           (yank)
           (slime-repl-send-input "\n")
           (switch-to-buffer-other-window buf-name))
          (t (message "Not exist *slime-repl ccl* buffer!")))
    ))

;; repl 自動起動
(defun skt:start-slime ()
  (unless (slime-connected-p)
    (save-excursion (slime))))

(provide 'init_slime)
;;; init_slime.el ends here
