;;; packages/agent-shell.el -*- lexical-binding: t; -*-

(map! :leader "o s" #'agent-shell-send-region)

(require 'acp)
(require 'agent-shell)
(use-package agent-shell
  :ensure t

  :init
  (map! :leader
        :desc "Toggle agent-shell buffer" "o `" #'agent-shell-toggle
        :desc "Open agent-shell: Claude" "o c" #'agent-shell-anthropic-start-claude-code
        :desc "Open agent-shell: OpenAI" "o o" #'agent-shell-openai-start-codex)
  :hook
  (agent-shell-mode . (lambda () (whitespace-mode -1)))

  :config
  (setopt agent-shell-tool-use-expand-by-default t)
  ;; (setopt agent-shell-thought-process-expand-by-default t)
  (setopt agent-shell-user-message-expand-by-default t)
  ;; (setopt agent-shell-highlight-blocks t)
  ;; (setopt agent-shell-highlight-blocks t)
  (setopt agent-shell-show-busy-indicator t)

  (setopt agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication
           :api-key
           (lambda () (getenv "ANTHROPIC_API_KEY"))))
  (setopt agent-shell-openai-authentication
          (agent-shell-openai-make-authentication
           :api-key
           (lambda () (getenv "OPENAI_API_KEY"))))
  (setopt agent-shell-google-authentication
          (agent-shell-google-make-authentication
           :api-key
           (lambda () (getenv "GOOGLE_API_KEY")))))
