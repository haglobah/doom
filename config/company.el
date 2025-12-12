;;; packages/company.el -*- lexical-binding: t; -*-

(after! company
  (keymap-unset company-active-map "<return>" t)
  (keymap-unset company-active-map "RET" t)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
  (define-key company-active-map (kbd "TAB") #'company-complete-selection))
