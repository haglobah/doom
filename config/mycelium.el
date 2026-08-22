;;; mycelium.el --- project wikilink cache -*- lexical-binding: t; -*-

;; Fontification (valid vs. missing links) and follow/create are handled by
;; markdown-mode's built-in wiki links. Our cache plugs into
;; `markdown-convert-wiki-link-to-filename', because the built-in resolver
;; only matches "<name>.<current buffer's extension>" — it can't resolve a
;; [[link]] from a .md file to a .mdx note (both mycelium and the garden mix
;; extensions), and its `project' search runs `directory-files-recursively'
;; per link during font-lock. The cache also ranks candidates for the company
;; backend in config/completion/markdown.el.

(defvar bah/markdown-files-cache nil
  "Cached hashmap of markdown filenames from the project.")

(defvar bah/markdown-cache-root nil
  "Project root the markdown cache was built from.
Cache values are paths relative to this root, so lookups must resolve
against it — not against whatever project the current buffer is in.")

(defvar bah/wikilink-reference-counts (make-hash-table :test 'equal)
  "Hashmap of wikilink name -> number of [[references]] across the project.")

(defun bah/count-wikilink-references (root files)
  "Count [[wikilink]] occurrences in the markdown FILES under ROOT.
Returns a hashmap of link name -> reference count."
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (file files)
      (when (string-match-p "\\.md\\(x\\)?$" file)
        (with-temp-buffer
          (insert-file-contents (doom-path root file))
          (goto-char (point-min))
          (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\]" nil t)
            (let ((name (match-string 1)))
              (puthash name (1+ (gethash name counts 0)) counts))))))
    counts))

(defun bah/rebuild-markdown-cache ()
  "Rebuild the markdown files cache from the project."
  (let* ((root (projectile-project-root))
         (files (projectile-project-files root))
         (markdown-map (make-hash-table :test 'equal)))
    (dolist (file files)
      (when (string-match-p "\\.md\\(x\\)?$" file)
        (let ((filename (file-name-sans-extension (file-name-nondirectory file))))
          (puthash filename file markdown-map))))
    (setq bah/markdown-files-cache markdown-map)
    (setq bah/markdown-cache-root root)
    (setq bah/wikilink-reference-counts (bah/count-wikilink-references root files))
    (message "[mycelium] Markdown cache rebuilt: %d files" (hash-table-count markdown-map))
    markdown-map))

(defun bah/get-project-markdown-file-names ()
  "Get a hashmap of markdown filenames from the current project.
Results are cached; rebuild on first access or after project changes."
  (or bah/markdown-files-cache
      (bah/rebuild-markdown-cache)))

(defun bah/refresh-wikilinks ()
  "Manually refresh the mycelium cache and refontify markdown buffers."
  (interactive)
  (bah/rebuild-markdown-cache)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'markdown-mode)
        (font-lock-flush)))))

(defun bah/mycelium-setup ()
  (bah/rebuild-markdown-cache))

(add-hook 'markdown-mode-hook #'bah/mycelium-setup)

;; Keep spaces in link targets — the notes have spaces in their filenames, and
;; the cache lookup above uses the raw link text anyway. This only affects the
;; fallback (creating a not-yet-existing note in the current directory).
(setq markdown-enable-wiki-links t
      markdown-wiki-link-fontify-missing t
      markdown-link-space-sub-char " ")

(defun bah/markdown-wiki-link-resolve (orig name)
  "Resolve wiki link NAME via the project cache, extension-agnostic.
Falls back to markdown-mode's resolution (NAME + current buffer's
extension, relative to the current directory) for new notes."
  (or (when-let* ((rel (gethash name (bah/get-project-markdown-file-names)))
                  (root bah/markdown-cache-root))
        (doom-path root rel))
      (funcall orig name)))

(advice-add 'markdown-convert-wiki-link-to-filename
            :around #'bah/markdown-wiki-link-resolve)

(map! :map markdown-mode-map
      :nvi "C-," #'markdown-follow-thing-at-point

      :leader
      :desc "Open or create wikilink" :nv "f ." #'markdown-follow-thing-at-point
      :desc "Refresh wikilinks" :nv "e r" #'bah/refresh-wikilinks
      :desc "Go to journal" :nv "e t" (cmd! (find-file "~/mycelium/2025.md")))
