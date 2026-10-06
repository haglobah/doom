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


  (map! :map difftastic-mode-map
        :nm "RET" #'+difftastic-visit-file
        :nm "C-j" #'+difftastic-visit-worktree-file
        [remap difftastic-diff-visit-file] #'+difftastic-visit-file
        [remap difftastic-diff-visit-worktree-file] #'+difftastic-visit-worktree-file))
