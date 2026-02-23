;;; config/keybindings/header.el -*- lexical-binding: t; -*-

(defun bah/in-garden-p ()
  "Check if the current buffer's file is in ~/beathagenlocher.com."
  (let ((dg-root (expand-file-name "~/beathagenlocher.com")))
    (and (buffer-file-name)
         (string-prefix-p dg-root (expand-file-name (buffer-file-name))))))

(defun bah/insert-file-header (fmt-string)
  "Insert a header at the top of the current buffer with title,
   growthStage, and dates."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((file-name (buffer-file-name))
           (title (if file-name
                      (file-name-base file-name)
                    "untitled"))
           (current-time (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))
      (insert (format fmt-string title current-time current-time)))))

(defun bah/insert-file-header-note ()
  (interactive)
  (unless (bah/in-garden-p)
    (user-error "Not in the beathagenlocher.com project"))
  (bah/insert-file-header "---
title: \"%s\"
description: \"\"
growthStage: seedling
startDate: %s
updated: %s
topics: []
publish: false
---

"))

(defun bah/insert-file-header-thought ()
  (interactive)
  (unless (bah/in-garden-p)
    (user-error "Not in the beathagenlocher.com project"))
  (bah/insert-file-header "---
title: \"%s\"
startDate: %s
topics: []
publish: true
---

"))

(defun bah/update-title ()
  "Update the title and updated fields of an existing file header.
Does not create a new header if one doesn't exist."
  (interactive)
  (unless (bah/in-garden-p)
    (user-error "Not in the beathagenlocher.com project"))
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "---\n")
        (let* ((file-name (buffer-file-name))
               (file-title (if file-name
                               (file-name-base file-name))))
          (if (search-forward "title: " nil t)
              (progn
                (delete-region
                 (progn (beginning-of-line) (point))
                 (progn (end-of-line) (point)))
                (insert (format "title: \"%s\"" file-title)))))
      (insert-file-header))))

(defun bah/update-date ()
  "Update the title and updated fields of an existing file header.
Does not create a new header if one doesn't exist."
  (interactive)
  (unless (bah/in-garden-p)
    (user-error "Not in the beathagenlocher.com project"))
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "---\n")
        (let* ((current-date (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))
          (if (search-forward "updated: " nil t)
              (progn
                (delete-region
                 (progn (beginning-of-line) (point))
                 (progn (end-of-line) (point)))
                (insert (format "updated: %s" current-date)))))
      (insert-file-header))))

(map! :map markdown-mode-map
      :localleader
      :prefix ("c" . "bah")
      :desc "Update date" "d" #'bah/update-date
      :desc "Update title" "t" #'bah/update-title)

(map! :map markdown-mode-map
      :localleader
      :prefix ("c h" . "Header")
      :desc "Insert Note header" "n" #'bah/insert-file-header-note
      :desc "Insert Thought header" "t" #'bah/insert-file-header-thought)

(defun bah/new-streamlet ()
  (interactive)
  (let* ((dg-root "~/beathagenlocher.com")
         (stream-folder (doom-path dg-root "src" "content" "stream"))
         (next-number (->> (directory-files stream-folder nil (rx ".mdx"))
                           (map 'list
                                (lambda (filename)
                                  (string-to-number (file-name-sans-extension filename))))
                           (sort)
                           (reverse)
                           (first)
                           (+ 1)))
         (next-streamlet-filename (concat (s-pad-left 5 "0" (number-to-string next-number)) ".mdx"))
         (next-streamlet-path (doom-path stream-folder next-streamlet-filename)))
    (with-temp-file next-streamlet-path (insert "stream"))
    (find-file next-streamlet-path)
    (move-end-of-line nil)
    (evil-append 0)))

(defun bah/dg-track ()
  "Run `just track` in the beathagenlocher.com project directory."
  (interactive)
  (unless (bah/in-garden-p)
    (user-error "Not in the beathagenlocher.com project"))
  (let ((default-directory (expand-file-name "~/beathagenlocher.com")))
    (shell-command "just track")))

(map! :leader
      :prefix ("d" . "digital garden")
      :desc "Writing: Streamlet" :nv "s" #'bah/new-streamlet
      :desc "Track" :nv "t" #'bah/dg-track)
