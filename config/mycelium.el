;;; mycelium.el ---                                     -*- lexical-binding: t; -*-

(defface bah/bracket-highlight
  '((t :foreground "#ff6b6b" :weight bold))
  "Face for highlighted content in [[...]]")

(defface bah/bracket-dim
  '((t :foreground "#666666"))
  "Face for dimmed content in [[...]]")

(defvar bah/markdown-files-cache nil
  "Cached hashmap of markdown filenames from the project.")

(defvar bah/markdown-files-cache-time nil
  "Timestamp of when the markdown files cache was last updated.")

(defvar bah/bracket-overlay-timer nil
  "Timer for debounced overlay updates.")

(defun bah/get-project-markdown-files ()
  "Get a hashmap of markdown filenames from the current project.
Results are cached for 5 seconds to avoid repeated filesystem scans."
  (let ((now (time-to-seconds (current-time))))
    (if (and bah/markdown-files-cache
             bah/markdown-files-cache-time
             (< (- now bah/markdown-files-cache-time) 5))
        bah/markdown-files-cache
      (let ((files (projectile-project-files (projectile-project-root)))
            (markdown-map (make-hash-table :test 'equal)))
        (dolist (file files)
          (when (string-match-p "\\.md\\(x\\)?$" file)
            (let ((filename (file-name-nondirectory file)))
              (puthash filename t markdown-map))))
        (setq bah/markdown-files-cache markdown-map)
        (setq bah/markdown-files-cache-time now)
        markdown-map))))

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

(add-hook 'markdown-mode-hook
  (lambda ()
    (bah/apply-bracket-overlays)
    ;; Re-apply overlays when buffer changes, but debounced
    (add-hook 'after-change-functions
      (lambda (_beg _end _len)
        (bah/debounced-apply-bracket-overlays))
      nil t)))
