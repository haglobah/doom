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

(defun bluesky-post-richtext (text.facets)
  "Post TEXT with FACETS to Bluesky as rich text"
  (let* ((text (car text.facets))
         (facets (cdr text.facets)))
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
                    (message "Bluesky post failed: %s" error-thrown))))))))

[
 (-> "[[Learning to C]]"
     (mdx->text.facets)
     (bluesky-post-richtext))
 ]

(defun mdx->text.facets (mdx)
  (interactive)
  (let* ((content mdx)
         (parsed (bluesky--parse-mdx-to-richtext content)))
    parsed))

(defun inspect (thing)
  (message thing)
  thing)

(defun mdx-buffer->text.facets ()
  (interactive)
  (inspect (mdx->text.facets (buffer-substring-no-properties (point-min) (point-max)))))

(defun mdx-region->text.facets (start end)
  (interactive "r")
  (mdx->text.facets (buffer-substring-no-properties start end)))

(defun publish-content ()
  (let* ((text.facets (mdx-buffer->text.facets)))
    (bluesky-post-richtext text.facets)))

(setq bluesky-handle "beathagenlocher.com")
(setq bluesky-app-password (getenv "BLUESKY_APP_SECRET"))

(map! :map markdown-mode-map
      :leader
      :prefix ("d b" . "dg bsky")
      :desc "Authenticate" "a" #'bluesky-authenticate
      :desc "Publish" "p" #'publish-content
      )
