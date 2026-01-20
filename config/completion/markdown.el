;;; markdown.el ---                                     -*- lexical-binding: t; -*-

(defun bah/wikilinks-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'bah/wikilinks-backend))
    (prefix (when (looking-back "\\[\\[\\([^]]*\\)" (line-beginning-position))
              (match-string 1)))
    (candidates (cl-remove-if-not
                 (lambda (candidate)
                   (string-prefix-p arg candidate t))
                 (hash-table-keys (bah/get-project-markdown-file-names))))))

(require 'company-emoji)

(after! markdown-mode
  (set-company-backend! 'markdown-mode 'company-emoji #'bah/wikilinks-backend))
