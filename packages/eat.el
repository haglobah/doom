;;; packages/eat.el -*- lexical-binding: t; -*-

(setq eat-shell (executable-find "fish")
      eat-kill-buffer-on-exit t)

(map! :leader
      :desc "Toggle other terminal window" "o t" #'eat-project-other-window
      :desc "Toggle terminal window" "o T" #'eat-project)

(map! :gnvi "C-j" #'eat-project-other-window)
(map! :map eat-char-mode-map :nvi "C-j" #'evil-quit)
(map! :map eat-semi-char-mode-map :nvi "C-j" #'evil-quit)
(map! :map eat-mode-map "C-j" #'evil-quit)
