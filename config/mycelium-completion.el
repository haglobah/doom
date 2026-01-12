;;; mycelium-completions.el ---                                     -*- lexical-binding: t; -*-

(defun bah/mycelium-company-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'bah/mycelium-company-backend))
    (prefix (when (looking-back "\\[\\[\\([^]]*\\)" (line-beginning-position))
              (match-string 1)))
    (candidates (cl-remove-if-not
                 (lambda (candidate)
                   (string-prefix-p arg candidate t))
                 (hash-table-keys (bah/get-project-markdown-file-names))))))

(after! markdown-mode
  (set-company-backend! 'markdown-mode #'bah/mycelium-company-backend))

