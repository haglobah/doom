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

(defvar bah/file-watch-descriptor nil
  "File watch descriptor for the project root.")

(defvar bah/bracket-overlay-timer nil
  "Timer for debounced overlay updates.")

(defun bah/invalidate-markdown-cache ()
  "Invalidate the markdown files cache."
  (setq bah/markdown-files-cache nil))

(defun bah/setup-file-watcher ()
  "Set up a file watcher on the project root to invalidate cache on changes."
  (let ((project-root (projectile-project-root)))
    (when project-root
      ;; Clean up old watcher if it exists
      (when bah/file-watch-descriptor
        (file-notify-rm-watch bah/file-watch-descriptor))

      ;; Set up new watcher
      (setq bah/file-watch-descriptor
            (file-notify-add-watch
             project-root
             '(change)
             (lambda (event)
               (let ((file (nth 2 event)))
                 ;; Only invalidate cache if a .md or .mdx file changed
                 (when (and file (string-match-p "\\.md\\(x\\)?$" file))
                   (bah/invalidate-markdown-cache)))))))))

(defun bah/get-project-markdown-file-names ()
  "Get a hashmap of markdown filenames from the current project.
Results are cached until the file watcher detects changes."
  (if bah/markdown-files-cache
      bah/markdown-files-cache
    (let ((files (projectile-project-files (projectile-project-root)))
          (markdown-map (make-hash-table :test 'equal)))
      (dolist (file files)
        (when (string-match-p "\\.md\\(x\\)?$" file)
          (let ((filename (file-name-sans-extension (file-name-nondirectory file))))
            (puthash filename file markdown-map))))
      (setq bah/markdown-files-cache markdown-map)
      markdown-map)))

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

(defun bah/debounced-apply-bracket-overlays ()
  "Debounced version of bah/apply-bracket-overlays.
Prevents excessive updates when typing rapidly."
  (when bah/bracket-overlay-timer
    (cancel-timer bah/bracket-overlay-timer))
  (setq bah/bracket-overlay-timer
        (run-with-idle-timer 0.5 nil #'bah/apply-bracket-overlays)))

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
            (find-file new-note-file))))))))

(add-hook 'markdown-mode-hook
  (lambda ()
    (bah/setup-file-watcher)
    (bah/apply-bracket-overlays)
    ;; Re-apply overlays when buffer changes, but debounced
    (add-hook 'after-change-functions
      (lambda (_beg _end _len)
        (bah/apply-bracket-overlays))
      nil t)))
