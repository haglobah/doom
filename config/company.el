;;; packages/company.el -*- lexical-binding: t; -*-

(after! company
  (keymap-unset company-active-map "<return>" t)
  (keymap-unset company-active-map "RET" t)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))
