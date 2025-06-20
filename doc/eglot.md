# eglot

```emacs-lisp
(leaf eglot
  ;; M-x package-install RET eglot RET
  ;; 最新をインストールしないと利用できない場合がある
  :ensure t
  :config
  ;; eglot-server-programs を明確に指定しておく
  (add-hook 'eglot-server-programs

            ;; python-ts-mode で pyright 利用
            ;; uv pip install pyright
            ;; M-! pyright --help が挙動する事
            ;; basedpyright を利用したい場合は pyright を basedpyright に変更
            '((python-mode python-ts-mode) .
              ("pyright-langserver" "--stdio"))

            ;; 参考 https://rust-analyzer.github.io/book/other_editors.html#eglot
            ;; M-! rust-analyzer --help が挙動する事
            '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy"))))
            )

  ;; eglot無効機能
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider ;; カーソル下のシンボルハイライト
          :inlayHintProvider ;; インラインヒント表示
          ))

  ;; eldoc echo を1行に抑止
  (setq eldoc-echo-area-use-multiline-p nil)
  )
```


```emacs-lisp
;; hook
(python-ts-mode-hook . eglot-ensure)
(rust-mode-hook . eglot-ensure)
```


```emacs-lisp
:bind (:eglot-mode-map
       ("C-c l a" . eglot-code-actions)
       ("C-c l d" . eglot-help-at-point)
       ("C-c l r" . eglot-rename)
       ("C-c l h" . eldoc)
       ("C-c l f" . eglot-format)
       ("C-c l F" . eglot-format-buffer)
       ("C-c l d" . xref-find-definitions-at-mouse)
       ("C-c l R" . eglot-reconnect))
```

