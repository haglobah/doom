;;; lang/astro/+markdown-injection.el -*- lexical-binding: t; -*-

;; Fontify the markdown living inside <Text is:raw>…</Text> islands.  The
;; astro grammar exposes an element's body as a bare (text) node; those
;; ranges are handed to the markdown grammar, whose (inline) nodes are in
;; turn handed to markdown-inline — the two-level split those grammars are
;; designed around.
;;
;; The markdown embeds must NOT be :local — Emacs 30's
;; treesit--update-ranges-local queries the *global* host parser, so a
;; second-level injection (markdown-inline into markdown) never sees local
;; first-level parsers.  Cost of the global parser: all Text islands are
;; parsed as one markdown document, so a construct at the very end of one
;; island can in principle bleed into the next.
;;
;; Not covered (fix when it itches): indentation and
;; treesit-language-at-point still treat the islands as astro text, so
;; TAB inside markdown uses the astro element rules.
;;
;; Kept free of Doom macros so it can be batch-tested standalone.

(require 'treesit)

(defvar +astro-markdown-text-tags '("Text")
  "Astro component tags whose text content is fontified as markdown.")

(defun +astro-markdown--range-rules ()
  "Range rules injecting markdown into `+astro-markdown-text-tags' bodies."
  (treesit-range-rules
   :embed 'markdown
   :host 'astro
   `((element
      (start_tag (tag_name) @_tag)
      (text) @cap
      (:match ,(concat "\\`" (regexp-opt +astro-markdown-text-tags) "\\'")
              @_tag)))

   :embed 'markdown-inline
   :host 'markdown
   '((inline) @cap)))

(defun +astro-markdown--font-lock-rules ()
  "Font-lock rules for the markdown and markdown-inline embeds."
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'md-heading
   '((atx_heading (atx_h1_marker) @markdown-header-delimiter-face
                  (inline) @markdown-header-face-1)
     (atx_heading (atx_h2_marker) @markdown-header-delimiter-face
                  (inline) @markdown-header-face-2)
     (atx_heading (atx_h3_marker) @markdown-header-delimiter-face
                  (inline) @markdown-header-face-3)
     (atx_heading (atx_h4_marker) @markdown-header-delimiter-face
                  (inline) @markdown-header-face-4)
     (atx_heading (atx_h5_marker) @markdown-header-delimiter-face
                  (inline) @markdown-header-face-5)
     (atx_heading (atx_h6_marker) @markdown-header-delimiter-face
                  (inline) @markdown-header-face-6)
     (setext_heading) @markdown-header-face-1)

   :language 'markdown
   :feature 'md-block
   '((thematic_break) @markdown-hr-face
     ([(list_marker_minus) (list_marker_plus) (list_marker_star)
       (list_marker_dot) (list_marker_parenthesis)]
      @markdown-list-face)
     (fenced_code_block (info_string) @markdown-language-info-face)
     (fenced_code_block (code_fence_content) @markdown-code-face)
     (fenced_code_block_delimiter) @markdown-markup-face
     (indented_code_block) @markdown-code-face
     (block_quote) @markdown-blockquote-face)

   :language 'markdown-inline
   :feature 'md-inline
   '((strong_emphasis) @markdown-bold-face
     (emphasis) @markdown-italic-face
     (code_span) @markdown-inline-code-face
     (inline_link (link_text) @markdown-link-face)
     (inline_link (link_destination) @markdown-url-face)
     (shortcut_link (link_text) @markdown-link-face)
     (full_reference_link (link_text) @markdown-link-face)
     ([(uri_autolink) (email_autolink)] @markdown-link-face))

   :language 'markdown-inline
   :feature 'md-markup
   :override t
   '((emphasis_delimiter) @markdown-markup-face
     (code_span_delimiter) @markdown-markup-face)))

(defun +astro-markdown-injection-setup ()
  "Highlight markdown inside `+astro-markdown-text-tags' elements.
Intended for `astro-ts-mode-hook'; a no-op when the markdown grammars
are missing (install them with `treesit-auto-install-all')."
  (when (and (treesit-ready-p 'markdown t)
             (treesit-ready-p 'markdown-inline t))
    (require 'markdown-mode)            ; supplies the faces
    (setq-local treesit-range-settings
                (append treesit-range-settings
                        (+astro-markdown--range-rules)))
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings
                        (+astro-markdown--font-lock-rules)))
    (setq-local treesit-font-lock-feature-list
                (let ((features (copy-tree treesit-font-lock-feature-list)))
                  (setf (nth 2 features)
                        (append (nth 2 features)
                                '(md-heading md-block md-inline md-markup)))
                  features))
    (treesit-font-lock-recompute-features)))

(add-hook 'astro-ts-mode-hook #'+astro-markdown-injection-setup)
