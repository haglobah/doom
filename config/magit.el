;;; magit.el ---                                     -*- lexical-binding: t; -*-

(map! :map magit-status-mode-map
      :nv "g r" #'magit-refresh)

(after! magit
  (setq magit-diff-refine-hunk 'all))
