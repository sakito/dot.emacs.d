# 利用しなくなった物などのメモ

# theme

```
;; (leaf emacs-theme
;;   :when window-system
;;   :config
;;   (add-to-list 'custom-theme-load-path
;;                (locate-user-emacs-file "lisp/theme"))
;;   (load-theme 'nightsblue t t)
;;   (enable-theme 'nightsblue)
;;   )

```

# emacsclient

teminalでの操作はNeovim利用のため
emacsclient利用しなくなったので、削除

```
(leaf server
  :doc "emacsclient を利用するためにサーバ起動
サーバが起動していた場合は先に起動していた方を優先"
  :require t
  :defun (server-running-p)
  :config
  (unless (server-running-p) (server-start))
  (defun my/raise-frame()
    ;; Frame を前面にする
    (raise-frame (selected-frame))
    ;; キーボードフォーカスを選択しているFrameにする
    (x-focus-frame (selected-frame)))
  :hook (
         (server-visit-hook . my/raise-frame)
         (find-file-hook . my/raise-frame)))
```

# ls-lisp

```
  ;; s で並び変え、C-u s で元に戻る
  ;; @see sorter.el
  ;; (leaf sorter
  ;;   :load-path* "lisp"
  ;;   :require t)

  ;; ;; システムのlsでなくls-lispを利用して表示
  ;; (leaf ls-lisp
  ;;   :require t
  ;;   :custom ((ls-lisp-use-insert-directory-program . nil)
  ;;            ;; ls のオプション
  ;;            (dired-listing-switches . "-lahF")
  ;;            ;; ディレクトリをより上に表示
  ;;            (ls-lisp-dirs-first . t)
  ;;            ))
```


# helm

 * helm-postframe: frameが画面に残ってしまう事があるので、利用断念

# font

```
(leaf font
  :doc "https://github.com/yuru7/Firge"
  :config
  (set-face-attribute 'default
                      nil
                      :family "Firge35"
                      :height 180)
  (set-frame-font "Firge35-18")
  (set-fontset-font nil
                    'unicode
                    (font-spec :family "Firge35")
                    nil
                    'append)
  ;; 古代ギリシア文字、コプト文字を表示したい場合は以下のフォントをインストールする
  ;; http://apagreekkeys.org/NAUdownload.html
  (set-fontset-font nil
                    'greek-iso8859-7
                    (font-spec :family "New Athena Unicode")
                    nil
                    'prepend)
  ;; 記号        3000-303F http://www.triggertek.com/r/unicode/3000-303F
  ;; 全角ひらがな 3040-309f http://www.triggertek.com/r/unicode/3040-309F
  ;; 全角カタカナ 30a0-30ff http://www.triggertek.com/r/unicode/30A0-30FF
  (set-fontset-font nil
                    '( #x3000 .  #x30ff)
                    (font-spec :family "Firge35")
                    nil
                    'prepend)
  ;; 半角カタカナ、全角アルファベット ff00-ffef http://www.triggertek.com/r/unicode/FF00-FFEF
  (set-fontset-font nil
                    '( #xff00 .  #xffef)
                    (font-spec :family "Firge35")
                    nil
                    'prepend)
  )
```

以上