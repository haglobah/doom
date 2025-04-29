;;; packages/rescript.el -*- lexical-binding: t; -*-

(after! rescript-mode
  (setq lsp-rescript-server-command
        '("rescript-language-server" "--stdio"))
  ;; Tell `lsp-mode` about the `rescript-vscode` LSP server
  (require 'lsp-rescript)
  ;; Enable `lsp-mode` in rescript-mode buffers
  (add-hook 'rescript-mode-hook 'lsp-deferred)
  ;; Enable display of type information in rescript-mode buffers
  (require 'lsp-ui)
  (add-hook 'rescript-mode-hook 'lsp-ui-doc-mode))
