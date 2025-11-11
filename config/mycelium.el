;;; mycelium.el ---                                     -*- lexical-binding: t; -*-

(require 'filenotify)

(defface bah/bracket-highlight
  '((t :inherit font-lock-type-face :weight bold))
  "Face for highlighted content in [[...]]")

(defface bah/bracket-dim
  '((t :inherit font-lock-constant-face))
  "Face for dimmed content in [[...]]")

(defvar bah/markdown-files-cache nil
  "Cached hashmap of markdown filenames from the project.")

(defvar bah/file-watchers (make-hash-table :test 'equal)
  "Map of project roots to their file watchers.")

(defun bah/rebuild-markdown-cache ()
  "Rebuild the markdown files cache from the project."
  (let ((files (projectile-project-files (projectile-project-root)))
        (markdown-map (make-hash-table :test 'equal)))
    (dolist (file files)
      (when (string-match-p "\\.md\\(x\\)?$" file)
        (let ((filename (file-name-sans-extension (file-name-nondirectory file))))
          (puthash filename file markdown-map))))
    (setq bah/markdown-files-cache markdown-map)
    (message "[mycelium] Markdown cache rebuilt: %d files" (hash-table-count markdown-map))
    markdown-map))

(defun bah/get-project-markdown-file-names ()
  "Get a hashmap of markdown filenames from the current project.
Results are cached; rebuild on first access or after project changes."
  (or bah/markdown-files-cache
      (bah/rebuild-markdown-cache)))

(defun bah/file-watcher-callback (event)
  "Callback for file system events in the project.
Invalidates cache on file creation/deletion."
  (let ((action (car event))
        (file (cadr event)))
    (when (and (string-match-p "\\.md\\(x\\)?$" file)
               (or (eq action 'created) (eq action 'deleted)))
      (setq bah/markdown-files-cache nil)
      (message "[mycelium] Cache invalidated by file event: %s" action)
      ;; Reapply overlays in all open markdown buffers
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (derived-mode-p 'markdown-mode)
            (bah/apply-bracket-overlays)))))))

(defun bah/setup-file-watcher ()
  "Set up file system watcher for the current project."
  (let ((project-root (projectile-project-root)))
    (unless (gethash project-root bah/file-watchers)
      (let ((watch-descriptor
             (file-notify-add-watch project-root
                                    '(change)
                                    #'bah/file-watcher-callback)))
        (puthash project-root watch-descriptor bah/file-watchers)
        (message "[mycelium] File watcher started for %s" project-root)))))

(defun bah/apply-bracket-overlays ()
  "Apply overlays to all [[...]] patterns in current buffer.
Highlights valid markdown file references, dims invalid ones."
  (interactive)

  ;; Remove old overlays with our marker
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'bah/bracket-overlay)
      (delete-overlay ov)))

  (let ((markdown-files (bah/get-project-markdown-file-names)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\]" nil t)
        (let* ((start (match-beginning 1))
               (end (match-end 1))
               (note-name (match-string 1))
               (is-valid (gethash note-name markdown-files))
               (overlay (make-overlay start end))
               (face (if is-valid 'bah/bracket-highlight 'bah/bracket-dim)))
          (overlay-put overlay 'face face)
          (overlay-put overlay 'bah/bracket-overlay t))))))

(defun bah/refresh-mycelium ()
  "Manually refresh the mycelium cache and reapply overlays.
Useful for testing and debugging."
  (interactive)
  (message "[mycelium] Manual refresh triggered")
  (setq bah/markdown-files-cache nil)
  (bah/rebuild-markdown-cache)
  (bah/apply-bracket-overlays)
  (message "[mycelium] Refresh complete"))

(defun bah/get-bracket-content-at-point ()
  "Get the content inside [[...]] at point, or nil if not inside brackets."
  (save-excursion
    (let ((start-pos (point)))
      ;; Try to find opening brackets before point
      (when (search-backward "[[" nil t)
        (let ((bracket-start (point)))
          ;; Try to find closing brackets after original point
          (goto-char start-pos)
          (when (search-forward "]]" nil t)
            (let ((bracket-end (point)))
              ;; Verify we're actually inside the brackets
              (if (and (> start-pos bracket-start) (< start-pos bracket-end))
                  (buffer-substring-no-properties (+ bracket-start 2) (- bracket-end 2))
                nil))))))))

(defun bah/open-or-create-bracket-file ()
  "Open the markdown file referenced in [[...]] at point.
If not inside brackets, message to minibuffer.
Looks for .mdx first, then .md. Creates file if it doesn't exist
in current directory."
  (interactive)
  (let ((note-name (bah/get-bracket-content-at-point)))
    (if (not note-name)
        (message "Only works inside double brackets [[...]]")
      (let* ((current-dir (file-name-directory (or (buffer-file-name) default-directory)))
             (markdown-files (bah/get-project-markdown-file-names))
             (project-note-file (gethash note-name markdown-files)))
        (if project-note-file
            (let* ((existing-note-file (doom-path (projectile-project-root) project-note-file)))
              (message "existing file path!")
              (find-file existing-note-file))
          (let* ((new-note-file (doom-path current-dir (concat note-name ".md"))))
            (with-temp-file new-note-file
              (insert ""))
            (find-file new-note-file)))))))

(add-hook 'markdown-mode-hook
  (lambda ()
    (bah/setup-file-watcher)
    (bah/rebuild-markdown-cache)
    (bah/apply-bracket-overlays)))
