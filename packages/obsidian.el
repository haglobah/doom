;;; packages/obsidian.el -*- lexical-binding: t; -*-

(use-package!
  ;; :defer

  :init
  ;; (obsidian-backlinks-mode t)
  :custom
  ;; location of obsidian vault
  (obsidian-directory "~/mycelium")
  ;; Default location for new notes from `obsidian-capture'
  (obsidian-inbox-directory "3_Resources")
  ;; Useful if you're going to be using wiki links
  (markdown-enable-wiki-links t)

  ;; These bindings are only suggestions; it's okay to use other bindings
  :bind (:map obsidian-mode-map
              ;; Create note
              ("C-c C-n" . obsidian-capture)
              ;; If you prefer you can use `obsidian-insert-wikilink'
              ("C-c C-l" . obsidian-insert-link)
              ;; Open file pointed to by link at point
              ("C-c C-o" . obsidian-follow-link-at-point)
              ;; Open a different note from vault
              ("C-c C-p" . obsidian-jump)
              ;; Follow a backlink for the current file
              ("C-c C-b" . obsidian-backlink-jump)))
