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

(defun bluesky--slugify (text)
  "Convert TEXT to a URL slug (github slugger style)"
  (downcase
   (replace-regexp-in-string
    "[^a-zA-Z0-9 -]" ""
    (replace-regexp-in-string " +" "-" (string-trim text)))))
[
 (bluesky--slugify "neo Vim")
 ]

(defun bluesky--strip-frontmatter (content)
  "Remove YAML frontmatter from CONTENT"
  (if (string-match "^---\n\\(\\(?:.\\|\n\\)*?\\)\n---\n\\(\\(?:.\\|\n\\)*\\)$" content)
      (match-string 2 content)
    content))
[
 (bluesky--strip-frontmatter "---\narst: hoho\ntitle: \"lala\"\n---\n  qwfp")
 ]

(defun bluesky--parse-mdx-to-richtext (content)
  "Parse MDX CONTENT and return (text . facets) for Bluesky rich text"
  (interactive)
  (let ((text (bluesky--strip-frontmatter content))
        (facets '())
        (byte-offset 0))

    ;; Process [[...]] wiki-style links first
    (while (string-match "\\[\\[\\([^]]+\\)\\]\\]" text)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (link-text (match-string 1 text))
             (slug (bluesky--slugify link-text))
             (url (concat bluesky-garden-base-url slug))
             ;; Calculate byte positions
             (byte-start (string-bytes (substring text 0 match-start)))
             (byte-end (+ byte-start (string-bytes link-text))))

        ;; Add facet for this link
        (push `((index (byteStart . ,byte-start)
                 (byteEnd . ,byte-end))
                (features . [(("$type" . "app.bsky.richtext.facet#link")
                              (uri . ,url))]))
              facets)

        ;; Replace [[link]] with just the link text
        (setq text (replace-match link-text nil nil text))))

    ;; Process [text](url) markdown-style links
    (while (string-match "\\[\\([^]]+\\)\\](\\([^)]+\\))" text)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (link-text (match-string 1 text))
             (url (match-string 2 text))
             ;; Calculate byte positions
             (byte-start (string-bytes (substring text 0 match-start)))
             (byte-end (+ byte-start (string-bytes link-text))))

        ;; Add facet for this link
        (push `((index (byteStart . ,byte-start)
                 (byteEnd . ,byte-end))
                (features . [(("$type" . "app.bsky.richtext.facet#link")
                              (uri . ,url))]))
              facets)

        ;; Replace [text](url) with just the link text
        (setq text (replace-match link-text nil nil text))))

    ;; Return text and facets
    (cons (string-trim text) (reverse facets))))

(defun encode-text (record-data)
  (json-encode `((collection . "app.bsky.feed.post")
                 (record . ,record-data))))

[
 (defvar text "
---
title: Raising Aspirations
startDate: 2025-09-05T18:28:27
topics: [Ambition]
publish: true
---

Just came upon this wonderful piece again:

[The high-return activity of raising others’ aspirations](https://marginalrevolution.com/marginalrevolution/2018/10/high-return-activity-raising-others-aspirations.html)

Already raised someone's aspirations today?")
 (defvar record (bluesky--parse-mdx-to-richtext text))
 (encode-text record)
 (bluesky--parse-mdx-to-richtext "[[Learning To C]]")
 (bluesky--parse-mdx-to-richtext "[hoho](https://qwfp.com)")
 ]

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
