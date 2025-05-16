;;; config/keybindings/markdown-link.el -*- lexical-binding: t; -*-

(defun bah/url->title|description (link)
  (with-temp-buffer
    (url-insert-file-contents link)
    (letrec
        ((dom (libxml-parse-html-region))
         (description-node
          (dom-search dom
                      (lambda (node)
                        (and (eq 'meta (car node))
                             (string-equal "description"
                                           (dom-attr node 'name))))))
         (title-node
          (dom-search dom
                      (lambda (node)
                        (and (eq 'title (car node))))))
         (description
          (dom-attr description-node 'content))
         (title
          (dom-text title-node)))
      (cond
       ((eq description nil) title)
       (t (concat title " x " description))))))

;; (bah/url->title|description "https://example.com")
;; (bah/url->title|description "https://beathagenlocher.com")

(defun doom/url-to-markdown-with-title (url)
  "Convert a bare URL to a Markdown link with its webpage title as the link text.
For example, https://github.com becomes [GitHub · Build and ship software...](https://github.com)"
  (interactive "sEnter URL: ")
  (message "Fetching title for %s..." url)
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("User-Agent" . "Doom Emacs/url-get-title"))))
    (url-retrieve url
                 (lambda (status)
                   (if (plist-get status :error)
                       (message "Error fetching URL: %s" (plist-get status :error))
                     (let ((title (doom-private/url-get-html-title)))
                       (if title
                           (let ((markdown-link (format "[%s](%s)" title url)))
                             (kill-new markdown-link)
                             (message "Copied to clipboard: %s" markdown-link))
                         (message "Could not find title for %s" url)))))
                 nil t t)))

(defun doom-private/url-get-html-title ()
  "Extract the title from the HTML in the current buffer."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (when (re-search-forward "<title\\>[^>]*>\\([^<]*\\)</title>" nil t)
      (let ((title (match-string 1)))
        ;; Clean up the title by removing extra whitespace
        (setq title (replace-regexp-in-string "[ \t\n\r]+" " " title))
        (string-trim title)))))

;; Alternative version that uses the DOM parser for more robust extraction
(defun doom-private/url-get-html-title-dom ()
  "Extract the title from the HTML in the current buffer using DOM parsing."
  (require 'dom)
  (goto-char (point-min))
  (when (re-search-forward "\n\n" nil t)
    (let* ((dom (libxml-parse-html-region (point) (point-max)))
           (title (dom-text (car (dom-by-tag dom 'title)))))
      (when title
        (setq title (replace-regexp-in-string "[ \t\n\r]+" " " title))
        (string-trim title)))))

;;;###autoload (autoload 'doom/url-at-point-to-markdown-title "tools/link-formatter/autoload" nil t)
(defun doom/url-at-point-to-markdown-title ()
  "Convert the URL at point to a Markdown link with the webpage title."
  (interactive)
  (let ((url (thing-at-point 'url)))
    (if url
        (doom/url-to-markdown-with-title url)
      (message "No URL found at point"))))

;;;###autoload (autoload 'doom/urls-in-region-to-markdown-titles "tools/link-formatter/autoload" nil t)
(defun doom/urls-in-region-to-markdown-titles (start end)
  "Convert all URLs in region to Markdown links with webpage titles."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "https?://[^\s\n]+" end t)
      (let ((url (match-string 0)))
        (message "Processing URL: %s" url)
        (doom/url-to-markdown-with-title url)))))
