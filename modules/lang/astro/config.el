;;; lang/astro/config.el -*- lexical-binding: t; -*-

(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package! astro-ts-mode
  :init
  (when (modulep! +lsp)
    (add-hook 'astro-ts-mode-hook #'lsp! 'append))
  :config
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe)))

(set-formatter! 'prettier-astro
  '("npx" "prettier" "--parser=astro"
    "--plugin=prettier-plugin-astro"
    (apheleia-formatters-indent "--use-tabs" "--tab-width" 'astro-ts-mode-indent-offset))
  :modes '(astro-ts-mode))

(use-package! lsp-tailwindcss
  :when (modulep! +lsp)
  :init
  (setq! lsp-tailwindcss-add-on-mode t)
  :config
  (add-to-list 'lsp-tailwindcss-major-modes 'astro-ts-mode))

;; Override after lsp-tailwindcss finishes loading so our registration wins.
(after! lsp-tailwindcss
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       (list (executable-find "tailwindcss-language-server") "--stdio")))
    :activation-fn #'lsp-tailwindcss--activate-p
    :server-id 'tailwindcss
    :priority -1
    :add-on? lsp-tailwindcss-add-on-mode
    :initialization-options #'lsp-tailwindcss--initialization-options
    :initialized-fn #'lsp-tailwindcss--company-dash-hack
    :notification-handlers (ht ("@/tailwindCSS/projectInitialized" #'ignore)
                               ("@/tailwindCSS/projectsDestroyed" #'ignore)))))

;; MDX Support
(add-to-list 'auto-mode-alist '("\\.\\(mdx\\)$" . markdown-mode))
(when (modulep! +lsp)
  (add-hook 'markdown-mode-local-vars-hook #'lsp! 'append))
