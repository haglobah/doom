;; -*- lexical-binding: t; -*-

(defun bah/read-unicode-char (c1 c2 c3 c4 _trailing_space_ignored)
  "Convert unicode input C1 C2 C3 C4 to the corresponding insert char call."
  (interactive "c\nc\nc\nc\nc")
  (insert-char (string-to-number (format "%c%c%c%c" c1 c2 c3 c4) 16)))

(map! :i "C-u" nil)
(map! "C-S-u" #'bah/read-unicode-char)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times.
Does not add to kill ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(map! :i "C-f" #'kill-word
      :i "C-<backspace>" #'doom/delete-backward-word
      :i "C-<delete>" #'delete-word)

(map! :nv "M-." (cmd! (affe-find "~")))

(map! :leader
      :desc "Restart Workspace" "c R" #'lsp-workspace-restart
      :desc "Show full error" "c h" #'flycheck-copy-errors-as-kill)

(map! :leader
      :desc "+format-buffer" "b f" #'+format/buffer)

(map! :nv
      :desc "Add new snippet" "C-S-y" #'+snippets/new
      :desc "Find snippet" "C-." #'+snippets/find-for-current-mode)


(defun bah/split-parent-window-right (&optional size)
  "Split the parent window into two side-by-side windows.
If there is no parent, splits the current window. Otherwise
identical to `split-window-right'."
  (interactive "P")
  (let ((old-window (selected-window))
        (size (and size (prefix-numeric-value size)))
        new-window)
    (when (and size (< size 0) (< (- size) window-min-width))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (split-window (window-parent) size t))
    ;; Always copy quit-restore parameter in interactive use.
    (let ((quit-restore (window-parameter old-window 'quit-restore)))
      (when quit-restore
        (set-window-parameter new-window 'quit-restore quit-restore)))
    new-window))

(map! :leader
      :desc "Add a parent right split to current window" :nv "w e" #'bah/split-parent-window-right
      :desc "vsplit window" :nv "w t" #'evil-window-vsplit
      :nv "w v" #'evil-window-top-left)

(map! :ni
      "S-<left>"  #'evil-window-left
      "S-<down>"  #'evil-window-down
      "S-<up>"    #'evil-window-up
      "S-<right>" #'evil-window-right

      "C-S-<left>"       #'+evil/window-move-left
      "C-S-<down>"       #'+evil/window-move-down
      "C-S-<up>"         #'+evil/window-move-up
      "C-S-<right>"      #'+evil/window-move-right)

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

(defun bah/insert-relative-file-path (file)
  "Prompt with `find-file` to choose FILE, then insert its path
relative to current buffer."
  (interactive "fInsert path to file: ")
  (let* ((current (or (buffer-file-name)
                      default-directory))
         (relative (concat "./" (file-relative-name (expand-file-name file)
                                                    (file-name-directory current)))))
    (insert relative)))

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
      :desc "Recent file" :nv "f R" #'consult-recent-file

      :desc "Insert relative path" :i

      :desc "direnv reload" :nv "d r" #'envrc-reload

      :desc "LSP: Format buffer" :nv "c f" #'lsp-format-buffer
      :desc "Toggle line wrapping" :v "v" #'visual-line-mode
      :desc "Copy file to clipboard" :nv "y" (cmd! (evil-ex-execute "%y+")))

(map! :nv "M-d" #'evil-mc-make-and-goto-next-match
      :nv "M-v" #'evil-mc-skip-and-goto-next-match)

;; sexp editing/movement
(defun bah/evil-eol-advice (&optional _count)
  (when (evil-eolp)
    (forward-char)))

(advice-add 'sp-forward-sexp :before #'bah/evil-eol-advice)

(map! "C-(" #'sp-backward-slurp-sexp
      "C-)" #'sp-forward-slurp-sexp
      "C-{" #'sp-backward-barf-sexp
      "C-}" #'sp-forward-barf-sexp

      "M-m" #'sp-raise-sexp
      "M-h" #'sp-absorb-sexp
      "M-k" #'sp-transpose-sexp

      "M-l" #'sp-beginning-of-sexp
      "M-'" #'sp-end-of-sexp

      "M-n" #'sp-backward-sexp
      "M-i" #'sp-down-sexp
      "M-e" #'sp-backward-up-sexp
      "M-o" #'sp-forward-sexp
      "M-u" #'sp-backward-down-sexp
      "M-y" #'sp-up-sexp)
