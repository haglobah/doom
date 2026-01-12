;;; -*- lexical-binding: t; -*-

(load! "../config/utils.el")
(load! "bluesky/mdx-to-richtext.el")

(defcustom bluesky-garden-base-url "https://beathagenlocher.com/"
  "Base URL for your digital garden"
  :type 'string
  :group 'bluesky)

(defun bsky-post-as-image (title text link)
  (interactive)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-encode `((title . ,title)
                                           (text . ,text)
                                           (link . ,link)
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
 (bsky-post-as-image "The title" "Hello there" "stream#00092")
 ]

(defun bsky-post (ret)
  "Post current buffer content to Bluesky."
  (interactive)
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-encode `((title . ,(ret-title ret))
                                           (text . ,(ret-text ret))
                                           (facets . ,(ret-facets ret))))
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
         (link (->> (file-relative-name (buffer-file-name) "~/beathagenlocher.com/")
                    (file-name-base)
                    (concat "stream#")
                    bah/i))
         (ret (bluesky--parse-mdx-to-richtext text))
         ;; NOTE: Maybe don't ignore the facets here?
         (sanitized (ret-text ret))
         (title (ret-title ret)))
    (bsky-post-as-image title sanitized link)))

(map! :map markdown-mode-map
      :leader
      :prefix ("d b" . "dg bsky")
      :desc "Post buffer as text" "b" #'bsky-post-buffer
      :desc "Post buffer as image" "i" #'bsky-post-buffer-as-image
      :desc "Post region" "r" #'bsky-post-region
      )
