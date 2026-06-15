;;; config/programming-language-specifics.el -*- lexical-binding: t; -*-

(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-sql-indent-offset 2)
(setq typescript-indent-level 2)

(setq treesit-language-source-alist
      '((css "https://github.com/tree-sitter/tree-sitter-css")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
(add-to-list 'auto-mode-alist '("\\.keymap\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.ct$" . lisp-mode))

;; SBCL comes from Nix without Quicklisp, so slynk-quicklisp fails to load.
(after! sly
  (setq sly-contribs (delq 'sly-quicklisp sly-contribs)))
