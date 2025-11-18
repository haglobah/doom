;; -*- lexical-binding: t; -*-

(use-package! aidermacs
  :config
  (setq! aidermacs-global-read-only-files '("~/mycelium/3_Resources/Coding/CONVENTIONS.md"))
  (setq! aidermacs-project-read-only-files '("CONVENTIONS.md"))
  (setq! aidermacs-program "aider")
  ;; (setq! aider-args '("--model" "gpt-4o-mini"))
  (map! :leader :n "r" #'aidermacs-transient-menu)
  (with-eval-after-load 'aidermacs
    (transient-replace-suffix
      'aidermacs-transient-menu
      "F" '("f" "Add Current File" aidermacs-add-current-file))
    (transient-replace-suffix
      'aidermacs-transient-menu
      "1" '("n" "Code Mode" aidermacs-switch-to-code-mode))
    (transient-replace-suffix
      'aidermacs-transient-menu
      "2" '("u" "Chat/Ask Mode" aidermacs-switch-to-ask-mode))
    (transient-replace-suffix
      'aidermacs-transient-menu
      "3" '("y" "Architect Mode" aidermacs-switch-to-architect-mode)))
  :custom
  (aidermacs-architect-mode t)
  (aidermacs-default-model "anthropic/claude-haiku-4-5-20251001")
  (aidermacs-architect-model "anthropic/claude-sonnet-4-5-20250929")
  (aidermacs-weak-model "anthropic/claude-haiku-4-5-20251001"))
