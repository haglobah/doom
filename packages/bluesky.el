;;; -*- lexical-binding: t; -*-

(load! "../config/utils.el")
(load! "bluesky/mdx-to-richtext.el")

(defcustom bluesky-garden-base-url "https://beathagenlocher.com/"
  "Base URL for your digital garden"
  :type 'string
  :group 'bluesky)

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

[Syndicated](https://indieweb.org/POSSE) from my [digital garden](https://beathagenlocher.com)"))

(defun bsky-post-region (start end)
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end)))
    (->> text
         (bluesky--parse-mdx-to-richtext)
         (bsky-post))))

(defun bsky-post-buffer ()
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (->> text
         (bsky-append-posse-backlink)
         (bluesky--parse-mdx-to-richtext)
         (bsky-post))))

[
 (->> "[[Learning to C]]"
      (bah/i)
      (bsky-post))
 ]

(map! :map markdown-mode-map
      :leader
      :prefix ("d b" . "dg bsky")
      :desc "Post buffer" "b" #'bsky-post-buffer
      :desc "Post region" "r" #'bsky-post-region
      )
