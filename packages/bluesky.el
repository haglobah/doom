;;; config/keybindings/bluesky.el -*- lexical-binding: t; -*-

(defcustom bluesky-handle ""
  "Your Bluesky handle (username.bsky.social)"
  :type 'string
  :group 'bluesky)

(defcustom bluesky-app-password ""
  "Your Bluesky app password"
  :type 'string
  :group 'bluesky)

(defvar bluesky--session nil
  "Current Bluesky session data")

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

(defun bluesky-post (text)
  "Post TEXT to Bluesky"
  (interactive "sPost to Bluesky: ")
  (unless bluesky--session
    (bluesky-authenticate))

  (when bluesky--session
    (let ((access-token (alist-get 'accessJwt bluesky--session))
          (did (alist-get 'did bluesky--session)))
      (request "https://bsky.social/xrpc/com.atproto.repo.createRecord"
        :type "POST"
        :headers `(("Authorization" . ,(format "Bearer %s" access-token))
                   ("Content-Type" . "application/json"))
        :data (json-encode `((repo . ,did)
                            (collection . "app.bsky.feed.post")
                            (record . ((text . ,text)
                                      (createdAt . ,(format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ"))))))
        :parser 'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (message "Posted to Bluesky successfully")))
        :error (cl-function
                (lambda (&rest args &key error-thrown &allow-other-keys)
                  (message "Bluesky post failed: %s" error-thrown)))))))

(defun bluesky-post-region (start end)
  "Post selected region to Bluesky"
  (interactive "r")
  (bluesky-post (buffer-substring-no-properties start end)))

(defun bluesky-post-buffer ()
  "Post entire buffer to Bluesky"
  (interactive)
  (bluesky-post (buffer-substring-no-properties (point-min) (point-max))))

(setq bluesky-handle "beathagenlocher.com")
(setq bluesky-app-password (getenv "BLUESKY_APP_SECRET"))
