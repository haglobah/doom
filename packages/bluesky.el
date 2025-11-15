;;; -*- lexical-binding: t; -*-

(load! "../config/utils.el")

(defcustom bluesky-garden-base-url "https://beathagenlocher.com/"
  "Base URL for your digital garden"
  :type 'string
  :group 'bluesky)

(defcustom bsky-post-url "https://localhost:3000/post"
  "URL for your local bsky-post server"
  :type 'string
  :group 'bsky)

(defun bsky-post (text)
  "Post current buffer content to Bluesky."
  (interactive)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (json-encode `((text . ,text)))))
    (url-retrieve
     url
     (lambda (status)
       (if (plist-get status :error)
           (message "Bsky post failed: %s" (plist-get status :error))
         (message "Posted to Bluesky"))))))

(defun bsky-post-region (start end)
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (bsky-post text)))

(defun bsky-post-buffer ()
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (bsky-post text)))

[
 (->> "[[Learning to C]]"
      (bah/i)
      (bsky-post))
 ]

(map! :map markdown-mode-map
      :leader
      :prefix ("d b" . "dg bsky")
      :desc "Publish" "p" #'publish-content
      )
