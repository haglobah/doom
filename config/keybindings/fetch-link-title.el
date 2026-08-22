;;; fetch-link-title.el --- Async URL title fetcher for markdown links -*- lexical-binding: t; -*-

(require 'dom)

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

(defun flt--extract-title (html)
  "Extract the <title> of HTML string; libxml decodes entities for us."
  (with-temp-buffer
    (insert html)
    (when-let* ((dom (libxml-parse-html-region (point-min) (point-max)))
                (title (string-trim (dom-text (car (dom-by-tag dom 'title))))))
      (unless (string-empty-p title)
        title))))

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
  (require 'plz)
  (let ((url (flt--get-url)))
    (if (not url)
        (message "No URL found at point or in kill ring.")
      (let ((marker (point-marker)))
        (message "Fetching title for %s..." url)
        (plz 'get url
          :as 'string
          :headers '(("User-Agent" . "Emacs"))
          :timeout 10
          :then (lambda (body)
                  (let ((title (flt--extract-title body)))
                    (if title
                        (progn
                          (flt--insert-markdown-link url title marker)
                          (message "Inserted: [%s](%s)" title url))
                      (message "Could not extract title from %s" url)))
                  (set-marker marker nil))
          :else (lambda (err)
                  (message "Fetch failed for %s: %s" url
                           (or (plz-error-message err)
                               (plz-error-curl-error err)
                               (plz-error-response err)))
                  (set-marker marker nil)))))))

(map! :leader
      :desc "->[title](link)" :nv "d l" #'flt-fetch-link-title)
