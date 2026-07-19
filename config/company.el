;;; packages/company.el -*- lexical-binding: t; -*-

;; Emacs 30 adds `ispell-completion-at-point' to text-mode buffers by default;
;; without a plain word-list dictionary it signals an error whenever consulted.
(setq text-mode-ispell-word-completion nil)

(after! company
  (keymap-unset company-active-map "<return>" t)
  (keymap-unset company-active-map "RET" t)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))
