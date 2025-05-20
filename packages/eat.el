;;; packages/eat.el -*- lexical-binding: t; -*-

(setq eat-shell (executable-find "fish")
      eat-kill-buffer-on-exit t)

(map! :leader
      :desc "Toggle other terminal window" "o t" #'eat-project-other-window
      :desc "Toggle terminal window" "o T" #'eat-project)
