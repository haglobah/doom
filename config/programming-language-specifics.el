;;; config/programming-language-specifics.el -*- lexical-binding: t; -*-

(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-sql-indent-offset 2)
(setq typescript-indent-level 2)

(setq treesit-language-source-alist
      '((astro "https://github.com/virchau13/tree-sitter-astro")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

(add-to-list 'auto-mode-alist '("\\.astro\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.keymap\\'" . c-mode))
