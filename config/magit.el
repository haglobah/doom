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
