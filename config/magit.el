;;; magit.el ---                                     -*- lexical-binding: t; -*-

(map! :map magit-status-mode-map
      :nv "g r" #'magit-refresh)

(map! :leader
      :desc "Commit all" :nv "g !" (cmd!
                                    (magit-commit-create (list "--all" "--message" "Add")))
      :desc "Amend all" :nv "g @" (cmd!
                                    (magit-commit-create (list "--all" "--amend" "--message" "Add"))))

(after! magit
  (setq magit-diff-refine-hunk 'all))
