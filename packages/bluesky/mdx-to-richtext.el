;;; mdx-to-richtext.el --- mdx to richtext           -*- lexical-binding: t; -*-

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
 (bluesky--strip-frontmatter
  "---
arst: hoho
title: \"lala\"
---
  qwfp")
 ]

(defun bluesky--parse-mdx-to-richtext (content)
  "Parse MDX CONTENT and return (text . facets) for Bluesky rich text"
  (interactive)
  (let ((text (-> content
                  (bluesky--strip-frontmatter)
                  (string-trim)))
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

[
 (bluesky--parse-mdx-to-richtext "Hi! https://qwfp.com wfp https://lala.com")
 (defvar text "
---
title: Raising Aspirations
startDate: 2025-09-05T18:28:27
topics: [Ambition]
publish: true
---

Just came upon this wonderful piece again:

[The high-return activity of raising others' aspirations](https://marginalrevolution.com/marginalrevolution/2018/10/high-return-activity-raising-others-aspirations.html)

Already raised someone's aspirations today?")
 (defvar record (bluesky--parse-mdx-to-richtext text))
 (encode-text record)
 (bluesky--parse-mdx-to-richtext "[[Learning To C]]")
 (bluesky--parse-mdx-to-richtext "[hoho](https://qwfp.com), and [here](https://github.com)")
 ]
