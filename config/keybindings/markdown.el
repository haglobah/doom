;;; config/keybindings/markdown.el -*- lexical-binding: t; -*-

;; Unbind M-t (`transpose-words') in markdown buffers. Binding to nil would only
;; remove markdown-mode-map's own entry and fall through to the global M-t, so we
;; shadow it with `undefined' (the command Emacs runs for any unbound key).
(map! :map markdown-mode-map
      "M-t" #'undefined)

;; Unbind M-h (`markdown-promote') in markdown buffers. Unlike M-t, this binding
;; doesn't come from markdown-mode-map (or the global map) -- `evil-markdown' puts
;; it in `evil-markdown-mode-map' for the normal+visual states, and a minor-mode
;; map outranks both the major-mode map and the global map. So shadowing it in
;; markdown-mode-map can't reach it. Bind it to nil *there* so the key falls
;; through to our global M-h (`bah/scroll-down-half-page'). `after!' guarantees
;; the keymap exists before we touch it.
(after! evil-markdown
  (map! :map evil-markdown-mode-map
        :nv "M-h" nil
        :nv "M-l" nil))
