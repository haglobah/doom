;; -*- lexical-binding: t; -*-

(use-package! aidermacs
  :config
  (setq! aidermacs-global-read-only-files '("~/mycelium/3_Resources/Coding/CONVENTIONS.md"))
  (setq! aidermacs-project-read-only-files '("CONVENTIONS.md"))
  ;; (setq! aider-args '("--model" "gpt-4o-mini"))
  (map! :g "C-c a" #'aidermacs-transient-menu)
  :custom
  (aidermacs-architect-mode t)
  (aidermacs-default-model "anthropic/claude-haiku-4-5-20251001"))
