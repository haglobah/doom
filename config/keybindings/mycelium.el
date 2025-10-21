;;; config/keybindings/mycelium.el -*- lexical-binding: t; -*-

(defun bah/format-url-as-markdown (url)
  (interactive "*p")
  "Fetch the title from URL and format as markdown link [title](url)."
  (bah/fetch-url-title url
    (lambda (title)
      (let ((formatted-title (or title "Link")))
        (insert (format "[%s](%s)" formatted-title url))))))

(defun bah/fetch-url-title (url callback)
  "Fetch the title from URL by parsing the HTML <title> tag."
  (request url
    :parser (lambda ()
              (goto-char (point-min))
              (when (search-forward "<title>" nil t)
                (let ((title-start (point))
                      (title-end (search-forward "</title>" nil t)))
                  (when title-end
                    (string-trim
                     (buffer-substring-no-properties title-start (- title-end 8)))))))
    :success (cl-function (lambda (&key data &allow-other-keys)
               (funcall callback data)))))

(bah/fetch-url-title "https://example.com" '(lambda (title) (message title)))

(request "https://google.com")

(let (body)
  (request
   "https://example.com/"
   :parser (lambda ()
              (goto-char (point-min))
              (when (search-forward "<title>" nil t)
                (let ((title-start (point))
                      (title-end (search-forward "</title>" nil t)))
                  (when title-end
                    (string-trim
                     (buffer-substring-no-properties title-start (- title-end 8)))))))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (setq body data)))
   :sync nil)
  body)
