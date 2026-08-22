;;; packages/super-save.el -*- lexical-binding: t; -*-

;; Replaces the old bah/save-buffer + focus-out/switch-buffer/switch-window
;; hook trio and `auto-save-visited-mode'. super-save 0.5 covers all of that
;; out of the box: buffer/window switches via `window-buffer-change-functions'
;; and `window-selection-change-functions' (which also fire for evil window
;; commands), focus loss via the non-obsolete `after-focus-change-function',
;; plus idle saving. It also skips remote (TRAMP) files, which the hand-rolled
;; version would have synced on every window switch.
;;
;; Doom's `use-package!' defers by default; hook off the first file visit
;; since there is nothing to save before that anyway.
(use-package! super-save
  :hook (doom-first-file . super-save-mode)
  :init
  (setq super-save-auto-save-when-idle t
        super-save-all-buffers t
        super-save-idle-duration 20
        super-save-remote-files nil
        super-save-silent t))
