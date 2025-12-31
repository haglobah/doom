;;; packages/agent-shell.el -*- lexical-binding: t; -*-

(require 'acp)
(require 'agent-shell)
(use-package agent-shell
  :ensure t

  :init
  (map! :leader
        :desc "Toggle agent-shell buffer" "o `" #'agent-shell-toggle
        :desc "Open agent-shell: Claude" "o c" #'agent-shell-anthropic-start-claude-code
        :desc "Open agent-shell: OpenAI" "o o" #'agent-shell-openai-start-codex)
  :config
  (setq! agent-shell-anthropic-authentication
         (agent-shell-anthropic-make-authentication
          :api-key
          (lambda () (getenv "ANTHROPIC_API_KEY"))))
  (setq! agent-shell-openai-authentication
         (agent-shell-openai-make-authentication
          :api-key
          (lambda () (getenv "OPENAI_API_KEY"))))
  (setq! agent-shell-google-authentication
         (agent-shell-google-make-authentication
          :api-key
          (lambda () (getenv "GOOGLE_API_KEY")))))
