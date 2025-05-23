;; -*- lexical-binding: t; -*-

(use-package! aider
  :config
  ;; (setq! aider-args '("--model" "gpt-4o-mini"))
  (setq! aider-args '("--model" "o4-mini"
                      "--no-git"
                      "--multiline"))
  (map! :g "C-c a" #'aider-transient-menu))
