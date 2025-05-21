;;; packages/gleam.el -*- lexical-binding: t; -*-
(use-package! gleam-ts-mode
  :mode (rx ".gleam" eos))

(add-to-list 'auto-mode-alist '("\\.gleam$" . gleam-ts-mode))

(setq lsp-gleam-executable '("gleam" "lsp"))

(after! gleam-ts-mode
  (unless (treesit-language-available-p 'gleam)
    (gleam-ts-install-grammar)))

(map! :map gleam-ts-mode-map
      :localleader
      :desc "Format" :nv "c f" #'gleam-ts-format
      :desc "Indent sexp" :nv "c i" #'prog-indent-sexp
      :desc "Start defun" :nv "g e" #'treesit-end-of-defun
      :desc "End defun" :nv "g a" #'treesit-beginning-of-defun)
