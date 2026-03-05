;;; fetch-link-title.el --- Async URL title fetcher for markdown links -*- lexical-binding: t; -*-

(defun flt--url-at-point ()
  "Get HTTP(S) URL at point, or nil."
  (when-let ((url (thing-at-point 'url)))
    (when (string-match-p "\\`https?://" url)
      url)))

(defun flt--url-from-kill-ring ()
  "Get URL from kill ring if it looks like one, or nil."
  (let ((text (current-kill 0 t)))
    (when (and text (string-match-p "\\`https?://" text))
      (string-trim text))))

(defun flt--get-url ()
  "Get URL from point first, then kill ring. Nil if neither has one."
  (or (flt--url-at-point)
      (flt--url-from-kill-ring)))

(defun flt--decode-html-entities (str)
  "Decode HTML entities in STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    ;; Named entities
    (while (re-search-forward "&amp;" nil t) (replace-match "&" t t))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t) (replace-match "<" t t))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t) (replace-match ">" t t))
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t) (replace-match "\"" t t))
    (goto-char (point-min))
    (while (re-search-forward "&apos;" nil t) (replace-match "'" t t))
    ;; Numeric entities (decimal and hex)
    (goto-char (point-min))
    (while (re-search-forward "&#x\\([0-9a-fA-F]+\\);" nil t)
      (replace-match (string (string-to-number (match-string 1) 16)) t t))
    (goto-char (point-min))
    (while (re-search-forward "&#\\([0-9]+\\);" nil t)
      (replace-match (string (string-to-number (match-string 1))) t t))
    (buffer-string)))

(defun flt--extract-title (html)
  "Extract <title> content from HTML string."
  (when (string-match "<title[^>]*>\\(\\(?:.\\|\n\\)*?\\)</title>" html)
    (let ((raw (string-trim (match-string 1 html))))
      (flt--decode-html-entities raw))))

(defun flt--insert-markdown-link (url title marker)
  "Replace URL at MARKER with a markdown link, or insert at MARKER."
  (when-let ((buf (marker-buffer marker)))
    (with-current-buffer buf
      (save-excursion
        (goto-char marker)
        (undo-boundary)
        (when-let ((url-bounds (thing-at-point-bounds-of-url-at-point)))
          (delete-region (car url-bounds) (cdr url-bounds)))
        (insert (format "[%s](%s)" title url))))))

(defun flt-fetch-link-title ()
  "Fetch the title of a URL (at point or kill ring) and insert a markdown link.
If point is on a URL, replaces it. Otherwise inserts at point."
  (interactive)
  (let ((url (flt--get-url)))
    (if (not url)
        (message "No URL found at point or in kill ring.")
      (let ((marker (point-marker))
            (buf (generate-new-buffer " *flt-curl*")))
        (message "Fetching title for %s..." url)
        (make-process
         :name "flt-curl"
         :buffer buf
         :command (list "curl" "-sL" "-m" "10"
                        "-r" "0-65536"
                        "-H" "User-Agent: Emacs"
                        "--" url)
         :sentinel
         (lambda (proc _event)
           (when (eq (process-status proc) 'exit)
             (if (/= (process-exit-status proc) 0)
                 (message "curl failed (exit %d) for %s"
                          (process-exit-status proc) url)
               (let ((title
                      (with-current-buffer (process-buffer proc)
                        (flt--extract-title (buffer-string)))))
                 (if title
                     (progn
                       (flt--insert-markdown-link url title marker)
                       (message "Inserted: [%s](%s)" title url))
                   (message "Could not extract title from %s" url))))
             (kill-buffer (process-buffer proc))
             (set-marker marker nil))))))))

(map! :leader
      :desc "->[title](link)" :nv "d f" #'flt-fetch-link-title)
