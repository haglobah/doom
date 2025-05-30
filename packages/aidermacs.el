;; -*- lexical-binding: t; -*-

(use-package! aidermacs
  :config
  ;; (setq! aider-args '("--model" "gpt-4o-mini"))
  (map! :g "C-c a" #'aidermacs-transient-menu)
  :custom
  (aidermacs-architect-mode t)
  (aidermacs-deault-model "sonnet"))
