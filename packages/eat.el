;;; packages/eat.el -*- lexical-binding: t; -*-

(setq eat-shell (executable-find "fish")
      eat-kill-buffer-on-exit t)

;; Globalized modes from config.el leak into terminal buffers and break the
;; cell grid: visual-line-mode re-wraps at spaces, emojify swaps emoji for
;; 47px images (two cells). Keep eat buffers plain.
(add-hook! 'eat-mode-hook
  (defun +beat/eat-plain-buffer-h ()
    (visual-line-mode -1)
    (setq-local truncate-lines t)))
(after! emojify
  (add-to-list 'emojify-inhibit-major-modes 'eat-mode))

(map! :leader
      :desc "Toggle other terminal window" "o t" #'eat-project-other-window
      :desc "Toggle terminal window" "o T" #'eat-project)

(map! :gnvi "C-j" #'eat-project-other-window)
(map! :map eat-char-mode-map :nvi "C-j" #'evil-quit)
(map! :map eat-semi-char-mode-map :nvi "C-j" #'evil-quit)
(map! :map eat-mode-map "C-j" #'evil-quit)
