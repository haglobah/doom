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
      (list (upcase key) (concat desc " (magit)") magit))))
