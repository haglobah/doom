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

(defun bluesky--get-frontmatter (content)
  "Get YAML frontmatter from CONTENT"
  (if (string-match "^---\n\\(\\(?:.\\|\n\\)*?\\)\n---\n\\(\\(?:.\\|\n\\)*\\)$" content)
      (match-string 1 content)
    nil))

(defun get-title (frontmatter)
  (when (and frontmatter
             (string-match "title:[ \t]*\"\\([^\"]+\\)\"" frontmatter))
    (match-string 1 frontmatter)))
[
 (->> "---
arst: hoho
title: \"lala\"
---
  qwfp"
      (bluesky--get-frontmatter)
      (get-title)
      )
 ]

(defun get-topics (frontmatter)
  (when (and frontmatter
             (string-match "topics:[ \t]*\\[\\([^]]+\\)\\]" frontmatter))
    (match-string 1 frontmatter)))

[
 (->> "---
arst: hoho
title: \"lala\"
topics: [hoho, lala, hihi]
---
  qwfp"
      (bluesky--get-frontmatter)
      (get-topics)
      )
 ]

(defun topic->tags (topic-string)
  (-as-> topic-string _
      (string-split _ "," t " ")
      (mapcar (lambda (topic) (string-replace " " "" topic)) _)
      (mapcar (lambda (topic) (concat "#" topic)) _)
      (string-join _ " ")))

[
 (split-string "hoho, lala" "," t " ")
 (-> "hoho, lala, Hi hi"
     (topic->tags))
 ]

(cl-defstruct ret
  title text facets tags)

(defun bluesky--parse-mdx-to-richtext (content)
  "Parse MDX CONTENT and return (text . facets) for Bluesky rich text"
  (interactive)
  (let* ((title (-> content (bluesky--get-frontmatter) (get-title)))
         (text (-> content
                   (bluesky--strip-frontmatter)
                   (string-trim)))
         (matches '())
         (tags (-> content
                     (bluesky--get-frontmatter)
                     (get-topics)
                     (topic->tags)))
         (pos 0))

    ;; Collect all [[...]] wiki-style links
    (while (string-match "\\[\\[\\([^]]+\\)\\]\\]" text pos)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (link-text (match-string 1 text))
             (slug (bluesky--slugify link-text))
             (url (concat bluesky-garden-base-url slug)))
        (push (list match-start match-end link-text url) matches)
        (setq pos match-end)))

    ;; Collect all [text](url) markdown-style links
    (setq pos 0)
    (while (string-match "\\[\\([^]]+\\)\\](\\([^)]+\\))" text pos)
      (let* ((match-start (match-beginning 0))
             (match-end (match-end 0))
             (link-text (match-string 1 text))
             (url (match-string 2 text)))
        (push (list match-start match-end link-text url) matches)
        (setq pos match-end)))

    ;; Sort matches by position (earliest first)
    (setq matches (sort matches (lambda (a b) (< (car a) (car b)))))

    ;; Process matches in order, building new text and facets
    (let ((new-text "")
          (facets '())
          (last-pos 0)
          (byte-offset 0))

      (dolist (match matches)
        (let* ((match-start (nth 0 match))
               (match-end (nth 1 match))
               (link-text (nth 2 match))
               (url (nth 3 match))
               ;; Add text before this match
               (before-text (substring text last-pos match-start))
               (byte-start (+ byte-offset (string-bytes before-text)))
               (byte-end (+ byte-start (string-bytes link-text))))

          ;; Append text before match
          (setq new-text (concat new-text before-text))
          ;; Append link text
          (setq new-text (concat new-text link-text))

          ;; Add facet
          (push `((index (byteStart . ,byte-start)
                   (byteEnd . ,byte-end))
                  (features . [(("$type" . "app.bsky.richtext.facet#link")
                                (uri . ,url))]))
                facets)

          ;; Update positions
          (setq byte-offset byte-end)
          (setq last-pos match-end)))

      ;; Append remaining text after last match
      (setq new-text (concat new-text (substring text last-pos)))

      ;; Return text and facets
      (make-ret :title title :text (string-trim new-text) :facets (reverse facets) :tags tags))))

[
 (bluesky--parse-mdx-to-richtext "[[a]] [b](b.com) [[c]]")
 (bluesky--parse-mdx-to-richtext "Hi! https://qwfp.com wfp https://lala.com")
 (defvar text "
---
title: \"Raising Aspirations\"
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
