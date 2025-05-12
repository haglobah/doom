;; -*- lexical-binding: t; -*-

(defun bah/read-unicode-char (c1 c2 c3 c4 _trailing_space_ignored)
  "Convert unicode input C1 C2 C3 C4 to the corresponding insert char call."
  (interactive "c\nc\nc\nc\nc")
  (insert-char (string-to-number (format "%c%c%c%c" c1 c2 c3 c4) 16)))

(map! :i "C-u" nil)
(map! "C-S-u" #'bah/read-unicode-char)

(map! :nv "M-." (cmd! (affe-find "~")))

(defun bah/evil-eol-advice (&optional _count)
  (when (evil-eolp)
    (forward-char)))

(advice-add 'sp-forward-sexp :before #'bah/evil-eol-advice)

(map! "C-(" #'sp-backward-slurp-sexp
      "C-)" #'sp-forward-slurp-sexp
      "C-{" #'sp-backward-barf-sexp
      "C-}" #'sp-forward-barf-sexp

      "M-m" #'sp-raise-sexp

      "M-n" #'sp-backward-sexp
      "M-e" #'sp-down-sexp
      "M-i" #'sp-backward-up-sexp
      "M-o" #'sp-forward-sexp
      "M-u" #'sp-backward-down-sexp
      "M-y" #'sp-up-sexp)

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

(map! :leader
      :desc "Move file" :nv "f m" #'doom/move-this-file
      :desc "Rename file" :nv "f r" #'bah/rename-current-buffer-file
      :desc "Recent file" :nv "f R" #'consult-recent-file

      :desc "direnv reload" :nv "d r" #'envrc-reload

      :desc "LSP: Format buffer" :nv "c f" #'lsp-format-buffer
      :desc "Toggle line wrapping" :v "v" #'visual-line-mode
      :desc "Copy file to clipboard" :nv "y" (cmd! (evil-ex-execute "%y+")))

(map! :nv "M-d" #'evil-mc-make-and-goto-next-match
      :nv "M-v" #'evil-mc-skip-and-goto-next-match)

(map! :desc "Insert current file name" "C-c f" (cmd! (insert (f-filename (file-name-sans-extension (buffer-file-name))))))

(map! :desc "Select whole line" :n "l" (kmacro "^ v $ <left>"))

(map! :n "_" (cmd! (insert " ") (evil-normal-state)))

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

(map! :leader :desc "Add a parent right split to current window" :nv "w e" #'bah/split-parent-window-right)
(map! :leader :nv "w t" #'evil-window-vsplit)
(map! :leader :nv "w v" #'evil-window-top-left)

(defun bah/duplicate-line-or-region-up (arg)
  "Duplicate the current line or selected region upward."
  (interactive "*p")
  (dotimes (_ arg)
    (bah/duplicate-line-or-region -1)))

(defun bah/duplicate-line-or-region-down (arg)
  "Duplicate the current line or selected region downward."
  (interactive "*p")
  (dotimes (_ arg)
    (bah/duplicate-line-or-region 1)))

(defun bah/duplicate-line-or-region (n)
  "Duplicate the current line or selected region upward or downward. The
direction is determined by the argument n being positive (i.e. downward)
or negative (i.e. upward). When a region is in play, the region will be kept
in a way so that this duplicate command can be replayed multiple times."
  (let* ((is-region (use-region-p))
         (beg (if is-region (region-beginning) nil))
         (end (if is-region (region-end) nil))
         (start (point))
         (region-other-point (if is-region (if (eq start beg) end beg)))
         (line-start (if is-region
                         (save-mark-and-excursion
                           (goto-char beg)
                           (line-beginning-position))
                       (line-beginning-position)))
         (line-end (if is-region
                       (save-mark-and-excursion
                         (goto-char end)
                         (line-end-position))
                     (line-end-position)))
         (text (concat (buffer-substring line-start line-end) "\n"))
         (forward (if (< 0 n) 1 -1))
         (shift (length text)))
    ;; If duplication direction is down (forward), keep the existing lines as
    ;; they appear, and insert before the current line. This makes it look as
    ;; if the selection has moved down, and can be used to keep duplicating
    ;; the selected lines downwards.
    (if (> forward 0)
        ;; Because other content will be inserted, save-mark-and-excursion won't
        ;; work. For that reason, I need to do some manual mark adjustment.
        (progn (goto-char line-start)
               (insert text)
               (goto-char (+ start shift))
               (when is-region
                 (save-excursion
                   (push-mark (+ region-other-point shift) 'nomsg nil))))

      ;; If duplication direction is up, insert the item below the current
      ;; line. This allows upward duplication to be repeated.
      (progn (goto-char line-end)
             (forward-line 1)
             (insert text)
             (goto-char start)
             (when is-region
               (save-excursion
                 (push-mark region-other-point 'nomsg nil)))))

    (setq deactivate-mark nil)))

(defun bah/move-line-or-region (arg)
  "Move the current line or region up or down by ARG lines.
With a selected region, move all lines that are
wholly or partially within the region.
Negative ARG moves up, positive ARG moves down."
  (interactive "*p")
  (let ((beg)
        (end)
        (region-active (use-region-p)))
    ;; Set up the range to move
    (if region-active
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))

    ;; Move the text
    (let ((col (current-column))
          (line-offset (if (< arg 0) -1 1)))
      (cond
       ;; Moving up but at buffer start
       ((and (< arg 0) (= beg (point-min)))
        (user-error "Beginning of buffer"))
       ;; Moving down but at buffer end
       ((and (> arg 0) (= end (point-max)))
        (user-error "End of buffer"))
       ;; Otherwise proceed
       (t
        (save-excursion
          (goto-char (if (< arg 0) beg end))
          (forward-line line-offset)
          (when (and (> arg 0) (eobp))
            (newline))
          (let ((text (delete-and-extract-region beg end)))
            (insert text)))
        (forward-line (* line-offset (if region-active
                                         (count-lines beg end)
                                       1)))
        (move-to-column col))))))

(map! "M-<up>" (cmd! (bah/move-line-or-region -1))
      "M-<down>" (cmd! (bah/move-line-or-region 1))
      "M-S-<up>" #'bah/duplicate-line-or-region-up
      "M-S-<down>" #'bah/duplicate-line-or-region-down)

;; vim
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(define-and-bind-text-object "-" "---" "---")
(define-and-bind-text-object "v" "$" "$")
;; (let ((var-string "[[:space:],\n\"\'\(\)\{\}\[]"))
;;   (define-and-bind-text-object "v" var-string var-string))

