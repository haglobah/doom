;;; config/keybindings/bluesky.el -*- lexical-binding: t; -*-

(defcustom bluesky-handle ""
  "Your Bluesky handle (username.bsky.social)"
  :type 'string
  :group 'bluesky)

(defcustom bluesky-app-password ""
  "Your Bluesky app password"
  :type 'string
  :group 'bluesky)

(defcustom bluesky-garden-base-url "https://beathagenlocher.com/"
  "Base URL for your digital garden"
  :type 'string
  :group 'bluesky)

(defvar bluesky--session nil
  "Current Bluesky session data")

(load! "./bluesky/mdx-to-richtext.el")

(defun encode-text (record-data)
  (json-encode `((collection . "app.bsky.feed.post")
                 (record . ,record-data))))

(defun bluesky-authenticate ()
  "Authenticate with Bluesky and store session"
  (interactive)
  (request "https://bsky.social/xrpc/com.atproto.server.createSession"
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode `((identifier . ,bluesky-handle)
                         (password . ,bluesky-app-password)))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (setq bluesky--session data)
                (message "Bluesky authentication successful")))
    :error (cl-function
            (lambda (&rest args &key error-thrown &allow-other-keys)
              (message "Bluesky auth failed: %s" error-thrown)))))

(defun bluesky-post-richtext (text facets)
  "Post TEXT with FACETS to Bluesky as rich text"
  (unless bluesky--session
    (bluesky-authenticate))

  (when bluesky--session
    (let ((access-token (alist-get 'accessJwt bluesky--session))
          (did (alist-get 'did bluesky--session))
          (record-data `((text . ,text)
                         (createdAt . ,(format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" (current-time) t)))))

      ;; Add facets if they exist
      (when facets
        (setq record-data (append record-data `((facets . ,(vconcat facets))))))

      (request "https://bsky.social/xrpc/com.atproto.repo.createRecord"
        :type "POST"
        :headers `(("Authorization" . ,(format "Bearer %s" access-token))
                   ("Content-Type" . "application/json"))
        :data (json-encode `((repo . ,did)
                             (collection . "app.bsky.feed.post")
                             (record . ,record-data)))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (message "Posted to Bluesky successfully with rich text")))
        :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys)
                  (message "Bluesky post failed: %s" error-thrown)))))))

(defun bluesky-post (text)
  "Post TEXT to Bluesky (plain text)"
  (interactive "sPost to Bluesky: ")
  (bluesky-post-richtext text nil))

(defun bsky-test-buffer ()
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (parsed (bluesky--parse-mdx-to-richtext content)))
    (message
     (cdr parsed))))

(defun bluesky-post-mdx-buffer ()
  "Post current MDX buffer to Bluesky with rich text formatting"
  (interactive)
  (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
         (parsed (bluesky--parse-mdx-to-richtext content))
         (text (car parsed))
         (facets (cdr parsed)))

    ;; Check Bluesky's character limit (300 chars)
    (if (> (length text) 300)
        (message "Text too long for Bluesky (%d chars, max 300)" (length text))
      (bluesky-post-richtext text facets))))

(defun bluesky-post-mdx-region (start end)
  "Post selected MDX region to Bluesky with rich text formatting"
  (interactive "r")
  (let* ((content (buffer-substring-no-properties start end))
         (parsed (bluesky--parse-mdx-to-richtext content))
         (text (car parsed))
         (facets (cdr parsed)))

    (if (> (length text) 300)
        (message "Text too long for Bluesky (%d chars, max 300)" (length text))
      (bluesky-post-richtext text facets))))

;; Keep the old functions for backwards compatibility
(defun bluesky-post-region (start end)
  "Post selected region to Bluesky (plain text)"
  (interactive "r")
  (bluesky-post (buffer-substring-no-properties start end)))

(defun bluesky-post-buffer ()
  "Post entire buffer to Bluesky (plain text)"
  (interactive)
  (bluesky-post (buffer-substring-no-properties (point-min) (point-max))))

(setq bluesky-handle "beathagenlocher.com")
(setq bluesky-app-password (getenv "BLUESKY_APP_SECRET"))


(map! :map markdown-mode-map
      :leader
      :prefix ("d b" . "dg bsky")
      :desc "Authenticate" "a" #'bluesky-authenticate
      :desc "Publish" "p" #'bluesky-post-mdx-buffer
      )
