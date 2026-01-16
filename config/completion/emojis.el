;;; emojis.el ---                                     -*- lexical-binding: t; -*-

(require 'company-emoji)
(after! markdown-mode
  (set-company-backend! 'markdown-mode 'company-emoji))
