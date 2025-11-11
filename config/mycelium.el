;;; mycelium.el ---                                     -*- lexical-binding: t; -*-

(defface bah/bracket-highlight
  '((t :foreground "#ff6b6b" :weight bold))
  "Face for highlighted content in [[...]]")

(defface bah/bracket-dim
  '((t :foreground "#666666"))
  "Face for dimmed content in [[...]]")

(defun bah/get-project-markdown-files ()
  "Get a hashmap of markdown filenames from the current project."
  (let ((files (projectile-project-files (projectile-project-root)))
        (markdown-map (make-hash-table :test 'equal)))
    (dolist (file files)
      (when (string-match-p "\\.md\\(x\\)?$" file)
        (let ((filename (file-name-nondirectory file)))
          (puthash filename t markdown-map))))
    markdown-map))

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

(add-hook 'markdown-mode-hook
  (lambda ()
    (bah/apply-bracket-overlays)
    ;; Re-apply overlays when buffer changes
    (add-hook 'after-change-functions
      (lambda (_beg _end _len)
        (bah/apply-bracket-overlays))
      nil t)))
