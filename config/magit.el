;;; magit.el ---                                     -*- lexical-binding: t; -*-

(map! :leader
      :desc "Commit all" :nv "g !" (cmd!
                                    (magit-commit-create (list "--all" "--message" "Add")))
      :desc "Amend all" :nv "g @" (cmd!
                                    (magit-commit-create (list "--all" "--amend" "--message" "Add")))
      :desc "Push" :nv "g p" (cmd! (magit-push)))

(after! magit
  (setq magit-diff-refine-hunk 'all)
  (setq magit-save-repository-buffers 'dontask))

;; Adds difftastic commands to the magit diff (d) and blame transients.
(use-package! difftastic
  :after magit
  :config
  ;; Keep difftastic's bindings elsewhere (blame, file dispatch, dired,
  ;; forge), but drop its M-d / M-c in the diff transient: the regular
  ;; keys there run difftastic instead (see below).
  (require 'difftastic-bindings)
  (setq difftastic-bindings-alist
        (seq-remove (lambda (entry)
                      (assq 'magit-diff (alist-get 'prefixes (car entry))))
                    difftastic-bindings-alist))
  (difftastic-bindings-mode)

  (defun +difftastic-diff-unstaged (&optional args files)
    "Like `magit-diff-unstaged', but with difftastic."
    (interactive (magit-diff-arguments))
    (difftastic--git-diff-range 'unstaged args files))

  (defun +difftastic-diff-staged (&optional args files)
    "Like `magit-diff-staged', but with difftastic."
    (interactive (magit-diff-arguments))
    (difftastic--git-diff-range 'staged (cons "--cached" args) files))

  (defun +difftastic-diff-working-tree (&optional rev args files)
    "Like `magit-diff-working-tree', but with difftastic."
    (interactive
     (cons (and current-prefix-arg
                (magit-read-branch-or-commit "Diff working tree and commit"))
           (magit-diff-arguments)))
    (difftastic--git-diff-range (or rev "HEAD") args files))

  (defun +difftastic-diff-dwim (&optional args files)
    "Like `difftastic-magit-diff', but show a commit at point with its header.
On a commit, `difftastic-magit-diff' diffs REV^..REV, which drops the
author, date and message; `difftastic-magit-show' keeps them."
    (interactive (magit-diff-arguments))
    (pcase (and (not (magit-section-match '(module module-commit)))
                (magit-diff--dwim))
      (`(commit . ,rev) (difftastic-magit-show rev))
      (_ (difftastic-magit-diff args files))))

  ;; In the diff (d) transient, the usual keys run difftastic; the same
  ;; key in uppercase runs the regular magit command.
  (pcase-dolist (`(,key ,desc ,difft ,magit)
                 '(("d" "Dwim" +difftastic-diff-dwim magit-diff-dwim)
                   ("r" "Diff range" difftastic-git-diff-range magit-diff-range)
                   ("u" "Diff unstaged" +difftastic-diff-unstaged magit-diff-unstaged)
                   ("s" "Diff staged" +difftastic-diff-staged magit-diff-staged)
                   ("w" "Diff worktree" +difftastic-diff-working-tree magit-diff-working-tree)
                   ("c" "Show commit" difftastic-magit-show magit-show-commit)))
    (transient-replace-suffix 'magit-diff key (list key desc difft))
    (transient-append-suffix 'magit-diff key
      (list (upcase key) (concat desc " (magit)") magit)))

  ;; Open full height on the right (65% of the frame), or in a tall
  ;; bottom window when the frame is too narrow for two columns.
  (defun +difftastic--side-p ()
    "Non-nil when the frame is wide enough for a side window."
    (>= (frame-width) 160))

  (defun +difftastic-requested-width ()
    "Width difft should format for: the window `+difftastic-display' makes."
    (- (if (+difftastic--side-p) (floor (* 0.65 (frame-width))) (frame-width))
       3))

  (defun +difftastic-display (buffer _requested-width)
    "Show BUFFER full height on the right, or in a tall bottom window."
    (pop-to-buffer
     buffer
     `((display-buffer-reuse-mode-window display-buffer-in-direction)
       (mode . difftastic-mode)
       (window . root)
       ,@(if (+difftastic--side-p)
             '((direction . right) (window-width . 0.65))
           '((direction . bottom) (window-height . 0.6))))))

  (setq difftastic-requested-window-width-function #'+difftastic-requested-width
        difftastic-display-buffer-function #'+difftastic-display)

  ;; +A −D after each file header, and the total in the header line.
  (load! "difftastic-stats")
  ;; Files grouped by intent, in review order, with claude's notes:
  ;; ]g / [g step through the groups, m marks one reviewed, C classifies,
  ;; g y copies the notes at point.
  (load! "difftastic-groups")
  ;; A gray bar and a checkbox on every hunk header (x ticks it green);
  ;; C-<down> / C-<up> jump between headers, TAB folds the hunk at point.
  (load! "difftastic-hunks")
  ;; Under the header line, bars with the part (group) and the file that
  ;; scrolled off the top; the next file's header slides under them.
  (load! "difftastic-sticky")

  ;; Visiting a file from the diff (RET, C-j) shows it in the window next
  ;; to the diff (usually magit), so the diff stays visible.  Closing the
  ;; file (kill or quit-window) puts that window's previous buffer back
  ;; and selects the diff again; quitting the diff then leaves magit.
  (defun +difftastic--visit (chunk-file &optional worktree)
    "Visit CHUNK-FILE beside the diff; closing it returns to the diff."
    (let* ((diff-win (selected-window))
           (target (get-mru-window (window-frame diff-win) nil t))
           (prev (when target
                   (pcase (window-parameter target '+difftastic-return)
                     ;; Visiting again from the same diff: keep the
                     ;; buffer from before the first visit.
                     (`(,(pred (eq (window-buffer target))) ,prev ,_) prev)
                     (_ (window-buffer target))))))
      (difftastic--diff-visit-file
       chunk-file
       (lambda (buf)
         (pop-to-buffer buf (if target
                                `((display-buffer-use-some-window)
                                  (some-window . ,(lambda (&rest _) target)))
                              '(nil (inhibit-same-window . t))))
         (set-window-parameter (selected-window) '+difftastic-return
                               (list buf prev diff-win))
         (with-current-buffer buf
           (add-hook 'kill-buffer-hook #'+difftastic--return-h nil t)))
       worktree)))

  (defun +difftastic--return-h ()
    "Restore windows that show this buffer from a diff visit."
    (let ((buf (current-buffer)))
      (dolist (win (window-list-1 nil nil t))
        (pcase (window-parameter win '+difftastic-return)
          (`(,(pred (eq buf)) ,prev ,diff-win)
           (set-window-parameter win '+difftastic-return nil)
           ;; Doom's `kill-current-buffer' has already switched the
           ;; selected window away from BUF, so accept that one too.
           (when (or (eq (window-buffer win) buf) (eq win (selected-window)))
             (when (buffer-live-p prev)
               (set-window-buffer win prev))
             (when (window-live-p diff-win)
               (select-window diff-win))))))))

  (defun +difftastic-visit-file (chunk-file)
    "Like `difftastic-diff-visit-file', but beside the diff."
    (interactive (list (difftastic--chunk-file-at-point)) difftastic-mode)
    (+difftastic--visit chunk-file))

  (defun +difftastic-visit-worktree-file (chunk-file)
    "Like `difftastic-diff-visit-worktree-file', but beside the diff."
    (interactive (list (difftastic--chunk-file-at-point)) difftastic-mode)
    (+difftastic--visit chunk-file t))

  (defun +difftastic-visit-file-or-group ()
    "On an overview line, go to its group; else visit the file beside the diff."
    (interactive nil difftastic-mode)
    (if (get-text-property (point) '+difftastic-groups-step)
        (+difftastic-groups-jump)
      (call-interactively #'+difftastic-visit-file)))

  ;; Doom's motion-state smart tab binds <tab> and runs `+fold/toggle'
  ;; whenever the line end is invisible, so it steals TAB on a hidden
  ;; chunk.  Bind <tab> here; the mode's normal-state map wins.
  (map! :map difftastic-mode-map
        :nm [tab] #'+difftastic-hunks-toggle
        :nm "C-<down>" #'difftastic-next-chunk
        :nm "C-<up>" #'difftastic-previous-chunk
        :nm "x" #'+difftastic-hunks-toggle-checked
        :nm "RET" #'+difftastic-visit-file-or-group
        :nm "] g" #'+difftastic-groups-next
        :nm "[ g" #'+difftastic-groups-previous
        :nm "m" #'+difftastic-groups-toggle-reviewed
        :nm "C" #'+difftastic-groups-classify
        :nm "g y" #'+difftastic-groups-copy-notes
        :nm "C-j" #'+difftastic-visit-worktree-file
        [remap difftastic-toggle-chunk] #'+difftastic-hunks-toggle
        [remap difftastic-diff-visit-file] #'+difftastic-visit-file
        [remap difftastic-diff-visit-worktree-file] #'+difftastic-visit-worktree-file)

  ;; difft marks changed tokens with bold red/green only.  Give them a
  ;; tinted background, and the rest of a changed line (the gaps between
  ;; tokens) a fainter one.  Colors derive from the theme.
  (defface +difftastic-removed '((t)) "Removed tokens in difftastic output.")
  (defface +difftastic-added '((t)) "Added tokens in difftastic output.")
  (defface +difftastic-removed-line '((t)) "Removed lines in difftastic output.")
  (defface +difftastic-added-line '((t)) "Added lines in difftastic output.")

  (defun +difftastic-set-faces (&rest _)
    (let ((bg (face-background 'default)))
      (pcase-dolist (`(,token ,line . ,base)
                     '((+difftastic-removed +difftastic-removed-line . magit-diff-removed)
                       (+difftastic-added +difftastic-added-line . magit-diff-added)))
        (let ((fg (face-foreground base nil t)))
          (set-face-attribute token nil :foreground fg
                              :background (doom-blend fg bg 0.25))
          (set-face-attribute line nil :extend t
                              :background (doom-blend fg bg 0.08))))))
  (+difftastic-set-faces)
  (add-hook 'doom-load-theme-hook #'+difftastic-set-faces)

  (dolist (colors (list difftastic-normal-colors-vector
                        difftastic-bright-colors-vector))
    (aset colors 1 '+difftastic-removed)
    (aset colors 2 '+difftastic-added))

  ;; ansi-color puts difft's colors in `font-lock-face', so read and write
  ;; that property rather than `face'.
  (defun +difftastic--faces-at (pos)
    "List of faces in `font-lock-face' at POS."
    (let ((f (get-text-property pos 'font-lock-face)))
      (if (keywordp (car-safe f)) (list f) (ensure-list f))))

  (defun +difftastic--line-face (pos)
    "Line face matching the color of the line number at POS, or nil."
    (let ((fg (seq-some (lambda (f) (and (consp f) (plist-get f :foreground)))
                        (+difftastic--faces-at pos))))
      (cond ((null fg) nil)
            ((equal fg (face-foreground '+difftastic-added)) '+difftastic-added-line)
            ((equal fg (face-foreground '+difftastic-removed)) '+difftastic-removed-line))))

  (defun +difftastic--tint (beg end face)
    "Add FACE between BEG and END wherever text has no background yet."
    (let ((pos beg))
      (while (< pos end)
        (let ((next (min end (next-single-property-change pos 'font-lock-face nil end)))
              (faces (+difftastic--faces-at pos)))
          (unless (seq-some (lambda (f) (and (consp f) (plist-member f :background)))
                            faces)
            (put-text-property pos next 'font-lock-face (append faces (list face))))
          (setq pos next)))))

  (defun +difftastic-tint-lines-h ()
    "Tint added and removed lines, using difftastic's own chunk parser."
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward (difftastic--chunk-regexp nil) nil t)
          (when-let* ((bounds (difftastic--chunk-bounds)))
            (if (eq (difftastic--classify-chunk bounds) 'single-column)
                (pcase-dolist (`((,_ ,eol) ,left ,right)
                               (difftastic--parse-single-column-chunk bounds))
                  (when-let* ((num (if (and right (+difftastic--line-face (nth 1 right)))
                                       right left))
                              (face (+difftastic--line-face (nth 1 num))))
                    (+difftastic--tint (nth 2 (or right left))
                                       (min (1+ eol) (point-max)) face)))
              (pcase-dolist (`((,_ ,eol) ,left ,right)
                             (difftastic--parse-side-by-side-chunk bounds))
                (when-let* ((left) (face (+difftastic--line-face (nth 1 left))))
                  (+difftastic--tint (nth 2 left) (if right (nth 1 right) eol) face))
                (when-let* ((right) (face (+difftastic--line-face (nth 1 right))))
                  (+difftastic--tint (nth 2 right) (min (1+ eol) (point-max)) face))))
            (goto-char (cdr bounds)))))))
  ;; This file loads as source.  Interpreted, the tinting loop rebuilds its
  ;; closures for every text run (~0.15s on a 400-line diff); compiled, it
  ;; takes ~0.02s.
  (mapc #'byte-compile '(+difftastic--faces-at +difftastic--line-face
                         +difftastic--tint +difftastic-tint-lines-h))
  (add-hook 'difftastic-mode-hook #'+difftastic-tint-lines-h))
