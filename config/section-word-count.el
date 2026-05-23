;;; section-word-count.el --- Word count overlays per markdown section -*- lexical-binding: t; -*-

(defface bah/section-word-count-face
  '((t :inherit font-lock-comment-face :height 0.85))
  "Face for section word-count overlays.")

(defconst bah/section-word-count--heading-regexp "^\\(#+\\) "
  "Matches markdown-style headings of any depth.")

(defcustom bah/section-word-count-idle-delay 0.3
  "Seconds of idle time before refreshing section word counts."
  :type 'number
  :group 'bah)

(defvar-local bah/section-word-count--timer nil)

(defun bah/section-word-count--delete-overlays ()
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'bah/section-word-count)
      (delete-overlay ov))))

(defun bah/section-word-count--format (count &optional preamble)
  (propertize
   (if preamble
       (format "[%d words preamble]\n" count)
     (format " [%d words]" count))
   'face 'bah/section-word-count-face))

(defun bah/section-word-count--annotate-heading (heading-eol body-start body-end)
  (let ((ov (make-overlay heading-eol heading-eol nil t nil)))
    (overlay-put ov 'bah/section-word-count t)
    (overlay-put ov 'after-string
                 (bah/section-word-count--format
                  (count-words body-start body-end)))))

(defun bah/section-word-count--annotate-preamble (body-end)
  (let ((count (count-words (point-min) body-end)))
    (when (> count 0)
      (let ((ov (make-overlay (point-min) (point-min) nil t nil)))
        (overlay-put ov 'bah/section-word-count t)
        (overlay-put ov 'before-string
                     (bah/section-word-count--format count t))))))

(defun bah/section-word-count--refresh ()
  (setq bah/section-word-count--timer nil)
  (save-excursion
    (save-restriction
      (widen)
      (bah/section-word-count--delete-overlays)
      (goto-char (point-min))
      (let ((first-heading (save-excursion
                             (when (re-search-forward
                                    bah/section-word-count--heading-regexp nil t)
                               (match-beginning 0)))))
        (bah/section-word-count--annotate-preamble
         (or first-heading (point-max))))
      (while (re-search-forward bah/section-word-count--heading-regexp nil t)
        (let* ((heading-eol (line-end-position))
               (body-start (min (1+ heading-eol) (point-max)))
               (body-end (save-excursion
                           (goto-char body-start)
                           (if (re-search-forward
                                bah/section-word-count--heading-regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
          (bah/section-word-count--annotate-heading
           heading-eol body-start body-end)
          (goto-char body-end))))))

(defun bah/section-word-count--schedule (&rest _)
  (when (timerp bah/section-word-count--timer)
    (cancel-timer bah/section-word-count--timer))
  (setq bah/section-word-count--timer
        (run-with-idle-timer bah/section-word-count-idle-delay nil
                             #'bah/section-word-count--refresh-in
                             (current-buffer))))

(defun bah/section-word-count--refresh-in (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when bah/section-word-count-mode
        (bah/section-word-count--refresh)))))

(defun bah/section-word-count--enable ()
  (add-hook 'after-change-functions #'bah/section-word-count--schedule nil t)
  (bah/section-word-count--refresh))

(defun bah/section-word-count--disable ()
  (remove-hook 'after-change-functions #'bah/section-word-count--schedule t)
  (when (timerp bah/section-word-count--timer)
    (cancel-timer bah/section-word-count--timer)
    (setq bah/section-word-count--timer nil))
  (bah/section-word-count--delete-overlays))

;;;###autoload
(define-minor-mode bah/section-word-count-mode
  "Show word count of each markdown-style section as an overlay."
  :lighter " §wc"
  (if bah/section-word-count-mode
      (bah/section-word-count--enable)
    (bah/section-word-count--disable)))

(provide 'section-word-count)

;;; section-word-count.el ends here
