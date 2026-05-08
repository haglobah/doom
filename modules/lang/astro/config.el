;;; lang/astro/config.el -*- lexical-binding: t; -*-

(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Register astro before treesit-auto consults its recipe list.
  ;; tsx + css are :requires because astro-ts-mode evaluates
  ;; (typescript-ts-mode--indent-rules 'tsx) inside a top-level defvar
  ;; (needs tsx grammar at *load* time), and pulls in css--treesit-*
  ;; during activation.
  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'astro
                :ts-mode 'astro-ts-mode
                :requires '(tsx css)
                :url "https://github.com/virchau13/tree-sitter-astro"
                :revision "master"
                :source-dir "src"))
  ;; treesit-auto-langs is a defcustom whose default snapshot was taken
  ;; before we extended the recipe list, so add astro explicitly.
  (cl-pushnew 'astro treesit-auto-langs)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Defer: requiring astro-ts-mode at boot crashes when the tsx grammar
;; is missing (see :requires comment above). With :defer t, the package
;; only loads when its autoload fires — i.e. when a .astro buffer is
;; opened, which only happens after astro-ts-mode-autoloads.el sees
;; (treesit-ready-p 'astro). Run `M-x treesit-auto-install-all' once on
;; a fresh setup to populate the grammars.
(use-package! astro-ts-mode
  :defer t
  :init
  (when (modulep! +lsp)
    (add-hook 'astro-ts-mode-hook #'lsp! 'append)))

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
  (add-to-list 'lsp-tailwindcss-major-modes 'astro-ts-mode)
  (lsp-register-custom-settings
   '(("tailwindCSS.files.exclude"
      ["**/.git/**" "**/.direnv/**" "**/node_modules/**" "**/.hg/**" "**/.svn/**"]))))

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

;; ;; MDX Support
;; (add-to-list 'auto-mode-alist '("\\.\\(mdx\\)$" . markdown-mode))
;; (when (modulep! +lsp)
;;   (add-hook 'markdown-mode-local-vars-hook #'lsp! 'append))
