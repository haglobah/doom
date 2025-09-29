;;; packages/agent-shell.el -*- lexical-binding: t; -*-

(use-package! agent-shell

  :init
  (map! :leader
        :desc "Open agent-shell: Claude" "o c" #'agent-shell-start-claude-code-agent
        :desc "Open agent-shell: OpenAI" "o o" #'agent-shell-start-codex-agent)
  :config
  (setq! agent-shell-anthropic-key (lambda () (getenv "ANTHROPIC_API_KEY")))
  (setq! agent-shell-openai-key (lambda () (getenv "OPENAI_API_KEY"))))
