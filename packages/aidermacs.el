;; -*- lexical-binding: t; -*-

(use-package! aidermacs
  :config
  ;; (setq! aider-args '("--model" "gpt-4o-mini"))
  (map! :g "C-c a" #'aidermacs-transient-menu)
  :custom
  (aidermacs-architect-mode t)
  (aidermacs-default-model "anthropic/claude-sonnet-4-5-20250929"))
