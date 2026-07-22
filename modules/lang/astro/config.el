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
  ;; :ext is needed for treesit-auto-add-to-auto-mode-alist to re-register
  ;; .astro after Doom's tools/tree-sitter :preface strips every *-ts-mode
  ;; entry from auto-mode-alist.
  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'astro
                :ts-mode 'astro-ts-mode
                :requires '(tsx css)
                :url "https://github.com/virchau13/tree-sitter-astro"
                :revision "master"
                :source-dir "src"
                :ext "\\.astro\\'"))
  ;; markdown-inline has no upstream recipe (there is no major mode for
  ;; it), but +markdown-injection needs the grammar. The fake :ts-mode is
  ;; inert: every remap/auto-mode-alist path in treesit-auto is
  ;; fboundp-guarded; only treesit-auto-install-all uses the recipe.
  (add-to-list 'treesit-auto-recipe-list
               (make-treesit-auto-recipe
                :lang 'markdown-inline
                :ts-mode 'markdown-inline-ts-mode
                :url "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                :source-dir "tree-sitter-markdown-inline/src"))
  ;; treesit-auto-langs is a defcustom whose default snapshot was taken
  ;; before we extended the recipe list, so add astro explicitly.
  (cl-pushnew 'astro treesit-auto-langs)
  (cl-pushnew 'markdown-inline treesit-auto-langs)
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

;; Markdown highlighting inside <Text> islands (see file for details).
(load! "+markdown-injection")

;; Prettier 3 no longer auto-discovers plugins; the project's .prettierrc
;; must list prettier-plugin-astro in `plugins`. With that in place,
;; --stdin-filepath is enough for prettier to pick parser and settings.
(set-formatter! 'prettier-astro
  '("npx" "prettier" "--stdin-filepath" filepath)
  :modes '(astro-ts-mode))

;; astro-ls hard-requires the typescript.tsdk init option. Upstream
;; lsp-astro only checks the project's node_modules and sends *nothing*
;; when it's absent, which makes the server reject initialize. Fall back
;; to the typescript on PATH (the project's nix shell, via envrc): tsc
;; lives at <store>/bin/tsc, its tsdk at <store>/lib/node_modules/typescript/lib.
(defadvice! +astro--tsdk-init-options-a ()
  :override #'lsp-astro--get-initialization-options
  (let* ((project-tsdk (f-join (lsp-workspace-root) "node_modules/typescript/lib"))
         (tsc (executable-find "tsc"))
         (path-tsdk (and tsc (f-join (f-parent (f-parent (file-truename tsc)))
                                     "lib/node_modules/typescript/lib")))
         (tsdk (seq-find #'file-exists-p (delq nil (list project-tsdk path-tsdk)))))
    (if tsdk
        `(:typescript (:tsdk ,tsdk))
      (lsp-warn "No typescript.tsdk for astro-ls: checked %s and tsc on PATH"
                project-tsdk))))

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
