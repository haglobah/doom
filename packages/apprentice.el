;;; packages/apprentice.el -*- lexical-binding: t; -*-

(use-package! apprentice
  :after elixir-mode
  :config

  (add-hook 'elixir-mode-hook 'apprentice-mode))
