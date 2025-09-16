;;; config/keybindings/header.el -*- lexical-binding: t; -*-

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
           (current-time (format-time-string "%Y-%m-%dT%H:%M:%S")))
      (insert (format fmt-string title current-time current-time)))))

(defun bah/insert-file-header-note ()
  (interactive)
  (bah/insert-file-header "---
title: %s
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
  (bah/insert-file-header "---
title: %s
startDate: %s
topics: []
publish: true
---

"))

(defun bah/update-file-header ()
  "Update the title and updated fields of an existing file header.
Does not create a new header if one doesn't exist."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "---\n")
        (let* ((file-name (buffer-file-name))
               (file-title (if file-name
                               (file-name-base file-name)))
               (current-date (format-time-string "%Y-%m-%dT%H:%M:%S")))
          (if (search-forward "title: " nil t)
              (progn
                (delete-region
                 (progn (beginning-of-line) (point))
                 (progn (end-of-line) (point)))
                (insert (format "title: %s" file-title))))
          (if (search-forward "updated: " nil t)
              (progn
                (delete-region
                 (progn (beginning-of-line) (point))
                 (progn (end-of-line) (point)))
                (insert (format "updated: %s" current-date)))))
        (insert-file-header))))

(map! :map markdown-mode-map
      :localleader "c h n" #'bah/insert-file-header-note
      :localleader "c h t" #'bah/insert-file-header-thought
      :localleader "c d" #'bah/update-file-header)

