;;; packages/just.el -*- lexical-binding: t; -*-

(use-package! justl
  :config
  (map! :map justl-compile-mode-map
        :n "e" 'justl-exec-recipe))

(use-package! just-mode)
