;; -*- lexical-binding: t; -*-
;; purescript.el

(use-package! purescript-mode
  :defer t
  :config
  (defun bah/purescript ()
    (turn-on-purescript-indentation))
  (add-hook! 'purescript-mode-hook #'bah/purescript))
