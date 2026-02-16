;;; -*- lexical-binding: t; -*-

(load! "../config/utils.el")
(load! "bluesky/mdx-to-richtext.el")

(defcustom bluesky-garden-base-url "https://beathagenlocher.com/"
  "Base URL for your digital garden"
  :type 'string
  :group 'bluesky)

;; TODO: Fix this function (json-readtable error 72)
(defun bsky--handle-response (status)
  "Handle Bluesky API response."
  (if (plist-get status :error)
      (message "Bsky post failed: %s" (plist-get status :error))
    (message "Posted to Bluesky: %s"
             (json-read-from-string
              (buffer-substring-no-properties (point) (point-max))))))

(defun bsky--post-json (endpoint payload)
  "POST PAYLOAD as JSON to ENDPOINT on localhost:3000."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string (json-encode payload) 'utf-8)))
    (url-retrieve (concat "http://localhost:3000" endpoint) #'bsky--handle-response)))

(defun bsky-post-as-image (text alttext link)
  (bsky--post-json "/post/as-image"
                   `((text . ,text) (alttext . ,alttext) (link . ,link))))

(defun bsky-post (ret)
  "Post parsed richtext RET to Bluesky."
  (bsky--post-json "/post"
                   `((title . ,(ret-title ret))
                     (text . ,(ret-text ret))
                     (facets . ,(ret-facets ret)))))

(defun bsky-append-posse-backlink (text)
  "Adds the POSSE backlink caption"
  (concat text "

Syndicated from my [digital garden](https://beathagenlocher.com)"))

(defun bsky--post-text (text)
  "Append backlink, parse to richtext, and post TEXT to Bluesky."
  (->> text
       (bsky-append-posse-backlink)
       (bluesky--parse-mdx-to-richtext)
       (bsky-post)))

(defun bsky-post-region (start end)
  (interactive "r")
  (bsky--post-text (buffer-substring-no-properties start end)))

(defun bsky-post-buffer ()
  (interactive)
  (bsky--post-text (buffer-substring-no-properties (point-min) (point-max))))

(defun bsky--buffer-link ()
  "Derive the Bluesky link slug from the current buffer's file path."
  (let* ((relative-file-name (file-relative-name
                              (buffer-file-name)
                              "~/beathagenlocher.com/src/content/"))
         (is-stream (string-prefix-p "stream/" relative-file-name)))
    (if is-stream
        (concat "stream#" (file-name-base relative-file-name))
      (file-name-base relative-file-name))))

(defun bsky--image-post-text (ret type-title)
  "Build the post text for an image post from RET.
When TYPE-TITLE is non-nil, prompt the user to edit the text."
  (let* ((title (ret-title ret))
         (tags (ret-tags ret))
         (default-text (concat title "\n\n" tags)))
    (if type-title
        (read-string "Post text: " default-text)
      default-text)))

(defun bsky-post-buffer-as-image (type-title)
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (ret (bluesky--parse-mdx-to-richtext text))
         (post-text (bsky--image-post-text ret type-title))
         (link (bsky--buffer-link)))
    (bsky-post-as-image post-text (ret-text ret) link)))

(defun bsky-post-buffer-as-image-title ()  (interactive)  (bsky-post-buffer-as-image nil))
(defun bsky-post-buffer-as-image-custom-text ()  (interactive)  (bsky-post-buffer-as-image t))

(map! :map markdown-mode-map
      :leader
      :prefix ("d b" . "dg bsky post")
      :desc "buffer: text" "b" #'bsky-post-buffer
      :desc "buffer: image, title" "i" #'bsky-post-buffer-as-image-title
      :desc "buffer: image, custom text" "t" #'bsky-post-buffer-as-image-custom-text
      :desc "region" "r" #'bsky-post-region)
