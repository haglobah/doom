;;; -*- lexical-binding: t; -*-

(load! "../config/utils.el")
(load! "bluesky/mdx-to-richtext.el")

(defcustom bluesky-garden-base-url "https://beathagenlocher.com/"
  "Base URL for your digital garden"
  :type 'string
  :group 'bluesky)

(defun bsky-post-as-image (text.link)
  (interactive)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-encode `((text . ,(car text.link))
                                           (link . ,(cdr text.link))
                                           )
                                         )
                            'utf-8)))
    (url-retrieve
     "http://localhost:3000/post/as-image"
     (lambda (status)
       (if (plist-get status :error)
           (message "Bsky post as image failed: %s" (plist-get status :error))
         (message "Posted to Bluesky: %s" status))))))
[
 (bsky-post-as-image (cons "Hello there" "stream#00092"))
 ]

(defun bsky-post (text.facets)
  "Post current buffer content to Bluesky."
  (interactive)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-encode `((text . ,(car text.facets))
                                           (facets . ,(cdr text.facets))))
                            'utf-8)))
    (url-retrieve
     "http://localhost:3000/post"
     (lambda (status)
       (if (plist-get status :error)
           (message "Bsky post failed: %s" (plist-get status :error))
         (message "Posted to Bluesky: %s" status))))))

[
 (->> "Is this finally a [nonbroken publish](https://github.com/haglobah/beathagenlocher.com/blob/53c04162ee0cd1ef12145b5b137055ffd9af921e/bsky-post-server/index.ts#L1) from [[Emacs]] by @beathagenlocher.com?"
      (bluesky--parse-mdx-to-richtext)
      (bsky-post))
 ]

(defun bsky-append-posse-backlink (text)
  "Adds the POSSE backlink caption"
  (concat text "

Syndicated from my [digital garden](https://beathagenlocher.com)"))

(defun bsky-post-region (start end)
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (->> text
         (bsky-append-posse-backlink)
         (bluesky--parse-mdx-to-richtext)
         (bsky-post))))

(defun bsky-post-buffer ()
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (->> text
         (bsky-append-posse-backlink)
         (bluesky--parse-mdx-to-richtext)
         (bsky-post))))

(defun bsky-post-buffer-as-image ()
  (interactive)
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         ;; TODO: src/content/....mdx -> stream#...
         (link (file-relative-name (buffer-file-name) "~/beathagenlocher.com"))
         (sanitized.facets (bluesky--parse-mdx-to-richtext text))
         ;; NOTE: Maybe don't ignore the facets here?
         (sanitized (car sanitized.facets)))
    (message sanitized)
    (message link)
    ))
;; (bsky-post-buffer-as-image (cons sanitized link))))

[
 (->> "[[Learning to C]]"
      (bah/i)
      (bsky-post))
 ]

(map! :map markdown-mode-map
      :leader
      :prefix ("d b" . "dg bsky")
      :desc "Post buffer" "b" #'bsky-post-buffer
      :desc "Post buffer" "i" #'bsky-post-buffer-as-image
      :desc "Post region" "r" #'bsky-post-region
      )
