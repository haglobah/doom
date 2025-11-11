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

(defun bah/get-project-markdown-files ()
  "Get a hashmap of markdown filenames from the current project.
Results are cached until the file watcher detects changes."
  (if bah/markdown-files-cache
      bah/markdown-files-cache
    (let ((files (projectile-project-files (projectile-project-root)))
          (markdown-map (make-hash-table :test 'equal)))
      (dolist (file files)
        (when (string-match-p "\\.md\\(x\\)?$" file)
          (let ((filename (file-name-nondirectory file)))
            (puthash filename t markdown-map))))
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

  (let ((markdown-files (bah/get-project-markdown-files)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\(.*?\\)\\]\\]" nil t)
        (let* ((start (match-beginning 1))
               (end (match-end 1))
               (content (match-string 1))
               (filename-with-ext (if (string-match-p "\\.md\\(x\\)?$" content)
                                      content
                                    (concat content ".md")))
               (is-valid (gethash filename-with-ext markdown-files))
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
Looks for .mdx first, then .md. Creates file if it doesn't exist."
  (interactive)
  (let ((content (bah/get-bracket-content-at-point)))
    (if (not content)
        (message "Only works inside double brackets [[...]]")
      (let* ((project-root (projectile-project-root))
             (mdx-file (expand-file-name (concat content ".mdx") project-root))
             (md-file (expand-file-name (concat content ".md") project-root))
             (target-file (cond
                           ((file-exists-p mdx-file) mdx-file)
                           ((file-exists-p md-file) md-file)
                           (t md-file))))  ; Default to .md if neither exists
        (unless (file-exists-p target-file)
          (make-directory (file-name-directory target-file) t)
          (write-region "" nil target-file))
        (find-file target-file)))))

(add-hook 'markdown-mode-hook
  (lambda ()
    (bah/setup-file-watcher)
    (bah/apply-bracket-overlays)
    ;; Re-apply overlays when buffer changes, but debounced
    (add-hook 'after-change-functions
      (lambda (_beg _end _len)
        (bah/apply-bracket-overlays))
      nil t)))
