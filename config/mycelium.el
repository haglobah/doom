;;; mycelium.el ---                                     -*- lexical-binding: t; -*-

(defface bah/bracket-highlight
  '((t :inherit font-lock-type-face :weight bold))
  "Face for highlighted content in [[...]]")

(defface bah/bracket-dim
  '((t :inherit font-lock-constant-face))
  "Face for dimmed content in [[...]]")

(defvar bah/markdown-files-cache nil
  "Cached hashmap of markdown filenames from the project.")

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

(defun bah/add-file-to-cache (file-path)
  "Add a markdown file to the cache by its path."
  (when (string-match-p "\\.md\\(x\\)?$" file-path)
    (let ((filename (file-name-sans-extension (file-name-nondirectory file-path))))
      (puthash filename file-path bah/markdown-files-cache))))

(defun bah/get-project-markdown-file-names ()
  "Get a hashmap of markdown filenames from the current project.
Results are cached; rebuild on first access or after project changes."
  (or bah/markdown-files-cache
      (bah/rebuild-markdown-cache)))

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
            (bah/rebuild-markdown-cache)
            (bah/apply-bracket-overlays)
            (add-hook 'after-change-functions
                      (lambda (_beg _end _len)
                        (bah/apply-bracket-overlays)))
                                        ;(add-hook 'after-save-hook #'bah/on-markdown-save nil t)
            ))

(defun bah/on-markdown-save ()
  "Hook to run on markdown file save.
Updates cache and reapplies overlays."
  (when (and (buffer-file-name)
             (string-match-p "\\.md\\(x\\)?$" (buffer-file-name)))
    (bah/add-file-to-cache (buffer-file-name))
    (bah/apply-bracket-overlays)))

(map! :map markdown-mode-map
      :nvi "C-," #'bah/open-or-create-bracket-file

      :leader
      :nv "f ." #'bah/open-or-create-bracket-file
      :nv "e r" #'bah/refresh-mycelium)

(defun bah/mycelium-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'bah/mycelium-company-backend))
    (prefix (when (looking-back "\\[\\[" 2)
              "[["))
    (candidates (let ((markdown-files (bah/get-project-markdown-file-names)))
                  (->> (hash-table-keys markdown-files)
                       (mapcar (lambda (file) (concat "[[" file))))))
    (meta (format "Markdown file: %s" arg))))

(add-to-list 'company-backends #'bah/mycelium-company-backend)
