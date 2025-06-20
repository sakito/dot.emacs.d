

```
(leaf lsp-mode
  :ensure t

  :custom
  (add-to-list 'lsp-disabled-clients 'pyls)

  (lsp-keymap-prefix . "C-c l")
  (lsp-document-sync-method . 'incremental)

  ;; server から値を「~*lsp-log*~」へ出力
  ;; 初期は t としておき、特に不要なら nil とする
  (lsp-log-io . t)

  ;; (lsp-inhibit-message . t)

  (lsp-enable-snippet . t)
  (lsp-enable-indentation . nil)

  (lsp-prefer-flymake . nil)
  (lsp-diagnostics-provider . :flycheck)

  (lsp-message-project-root-warning . t)
  (create-lockfiles . nil)
  (lsp-file-watch-threshold .nil)
  (lsp-signature-auto-activate . t)
  (lsp-signature-doc-lines . 1)
  (lsp-print-performance . t)
  (lsp-eldoc-render-all . t)
  (lsp-enable-completion-at-point . t)
  (lsp-enable-xref . t)
  (lsp-keep-workspace-alive . nil)
  (lsp-server-trace . nil)
  (lsp-auto-guess-root . nil)
  (lsp-document-sync-method . 2)
  (lsp-response-timeout . 5)
  (lsp-idle-delay . 0.500)
  (lsp-enable-file-watchers . nil)
  (lsp-headerline-breadcrumb-segments . '(symbols))
  (lsp-print-performance  . nil)
  ;; sever trace
  (lsp-server-trace . nil)
  (lsp-auto-configure . t)

  (lsp-headerline-breadcrumb-enable . t)
  ;; eldoc 表示設定
  ;; nil signatureのみ
  ;; t doc-string全体表示
  (lsp-eldoc-render-all . nil)

  (lsp-idle-delay . 0.5)
  (lsp-response-timeout . 5)
  (lsp-enable-folding . t)
  (lsp-enable-indentation . t)
  (lsp-completion-enable . nil)
  (lsp-completion-provider . :none)
  (lsp-completion-show-detail . nil)
  (lsp-enable-symbol-highlighting . t)
  (lsp-signature-render-documentation . nil)


  ;; :hook
  ;; (python-ts-mode-hook . lsp-deferred)
  ;; (rust-ts-mode-hook . lsp-deferred)

  :config
  (leaf lsp-ui
    :doc "ls-mode 用 UI"
    :ensure t
    :after lsp-mode markdown-mode

    :custom
    (lsp-ui-doc-enable . t)
    (lsp-ui-doc-deley . 0.5)
    (lsp-ui-doc-header . t)
    (lsp-ui-doc-include-signature . t)
    (lsp-ui-doc-position . 'at-point)
    (lsp-ui-doc-max-width . 150)
    (lsp-ui-doc-max-height . 30)
    (lsp-ui-doc-use-childframe . nil)
    (lsp-ui-doc-use-webkit . nil)
    (lsp-ui-flycheck-enable . t)
    (lsp-ui-peek-enable . t)
    (lsp-ui-peek-peek-height . 20)
    (lsp-ui-peek-list-width . 50)
    ;; never, on-demand, always
    (lsp-ui-peek-fontify . 'on-demand)

    :hook
    (lsp-mode-hook . lsp-ui-mode)

    )

  )
```
