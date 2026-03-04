;;; fetch-link-title.el --- Async URL title fetcher for markdown links -*- lexical-binding: t; -*-

(defun flt--url-at-point ()
  "Get URL at point, or nil."
  (thing-at-point 'url))

(defun flt--url-from-kill-ring ()
  "Get URL from kill ring if it looks like one, or nil."
  (let ((text (current-kill 0 t)))
    (when (and text (string-match-p "\\`https?://" text))
      (string-trim text))))

(defun flt--get-url ()
  "Get URL from point first, then kill ring. Nil if neither has one."
  (or (flt--url-at-point)
      (flt--url-from-kill-ring)))

(defun flt--extract-title (html)
  "Extract <title> content from HTML string."
  (when (string-match "<title[^>]*>\\([^<]*\\)</title>" html)
    (string-trim (match-string 1 html))))

(defun flt--insert-markdown-link (url title marker)
  "Replace URL at MARKER with a markdown link, or insert at MARKER."
  (with-current-buffer (marker-buffer marker)
    (save-excursion
      (goto-char marker)
      ;; If there's a raw URL at point, replace it
      (let ((url-bounds (thing-at-point-bounds-of-url-at-point)))
        (if url-bounds
            (progn
              (delete-region (car url-bounds) (cdr url-bounds))
              (insert (format "[%s](%s)" title url)))
          (insert (format "[%s](%s)" title url)))))))

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
                        "-H" "User-Agent: Emacs"
                        "--" url)
         :sentinel
         (lambda (proc _event)
           (when (eq (process-status proc) 'exit)
             (let ((title
                    (with-current-buffer (process-buffer proc)
                      (flt--extract-title (buffer-string)))))
               (if title
                   (progn
                     (flt--insert-markdown-link url title marker)
                     (message "Inserted: [%s](%s)" title url))
                 (message "Could not extract title from %s" url)))
             (kill-buffer (process-buffer proc))
             (set-marker marker nil))))))))

(map! :leader
      :desc "link->w/ title" :nv "d f" #'flt-fetch-link-title)
