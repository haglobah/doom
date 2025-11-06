;;; files.el ---                                     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Beat Hagenlocher

;; Author: Beat Hagenlocher <beat@gondor>
;; Keywords: c,

(defun bah/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun bah/insert-relative-file-path ()
  "Insert a file path relative to the project root, using Consult for completion."
  (interactive)
  (let* ((root (or (project-root (project-current))
                   default-directory))
         (files (directory-files-recursively
                 root ".*" nil (lambda (file)
                                 (not (string-match-p "/\\.[^/]*$" file)))))
         (rel-files-display
          (when files
            (->
             (mapcar (lambda (path)
                       (cons (file-relative-name path default-directory) path))
                     files)
             (sort (lambda (a b)
                     (< (length (car a)) (length (car b)))))))))
    (insert (projectile-completing-read "Choose file:" rel-files-display))))

(map! :desc "Insert current file name" "C-c f c"
      (cmd! (insert (f-filename (file-name-sans-extension (buffer-file-name)))))

      :desc "Open link" "C-c C-o" #'markdown-follow-link-at-point
      :desc "Insert relative file path" "C-c f i" #'bah/insert-relative-file-path)

(defun bah/move-file-to-dir-fuzzy ()
  "Move the current file to a directory chosen via fuzzy search.
If in a project, include all subdirectories (excluding dot-dirs),
displaying them relative to the project root.
Otherwise fall back to default `consult-dir` sources."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (require 'consult-dir)
  (let* ((proj (when (fboundp 'project-current)
                 (ignore-errors (project-current))))
         (proj-root (when proj (expand-file-name (project-root proj))))
         (default-directory (or proj-root default-directory))
         ;; Collect subdirectories under project root, skipping dotdirs
         (proj-dirs
          (when proj-root
            (directory-files-recursively
             proj-root
             ".*" t
             (lambda (dir)
               (not (string-match-p "/\\.[^/]*$" dir))))))
         ;; Keep only directories
         (proj-dirs (when proj-dirs (seq-filter #'file-directory-p proj-dirs)))
         ;; Convert to (display . realpath) pairs for consult
         (proj-dirs-display
          (when proj-dirs
            (->
             (mapcar (lambda (path)
                       (cons (file-relative-name path proj-root) path))
                     proj-dirs)
             (sort (lambda (a b)
                     (< (length (car a)) (length (car b))))))))
         ;; Temporary consult-dir source
         (source-project-subdirs
          (when proj-root
            `(:name ,(format "Project dirs (%s)" (file-name-nondirectory (directory-file-name proj-root)))
              :narrow ?P
              :category file
              :face consult-file
              :items ,proj-dirs-display))))
    (let* ((consult-dir-sources
            (if source-project-subdirs
                (cons source-project-subdirs consult-dir-sources)
              consult-dir-sources))
           (dir (consult-dir--pick "Move to directory: "))
           (basename (file-name-nondirectory buffer-file-name))
           (target-dir (expand-file-name dir))
           (target (expand-file-name basename target-dir)))
      (unless (file-directory-p target-dir)
        (make-directory target-dir t))
      (when (file-exists-p target)
        (user-error "File already exists at destination"))
      (rename-file buffer-file-name target)
      (set-visited-file-name target t t)
      (message "Moved %s → %s" basename target-dir))))

(map! :leader
      :desc "Move file" :nv "f m" #'bah/move-file-to-dir-fuzzy
      :desc "Move file (doom)" :nv "f M" #'doom/move-this-file
      :desc "Rename file" :nv "f r" #'bah/rename-current-buffer-file
      :desc "Recent file" :nv "f R" #'consult-recent-file)
