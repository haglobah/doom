;;; packages/company.el -*- lexical-binding: t; -*-

(after! company
  (keymap-unset company-active-map "<return>" t)
  (define-key company-active-map (kbd "<tab>") #'company-complete-selection))
