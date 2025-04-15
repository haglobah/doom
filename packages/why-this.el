;;; packages/why-this.el -*- lexical-binding: t; -*-

(use-package! why-this
  :defer

  :init
  (map! :leader :desc "Toggle inline git blame" :nv "b w" #'why-this-mode)

  :config
  (setq! why-this-idle-delay 0.01)
  (set-face-attribute 'why-this-face nil :foreground "gray" :slant 'oblique))
