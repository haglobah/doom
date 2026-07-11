;;; markdown.el ---                                     -*- lexical-binding: t; -*-

(defun bah/wikilinks-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'bah/wikilinks-backend))
    (prefix (when (looking-back "\\[\\[\\([^]]*\\)" (line-beginning-position))
              (match-string 1)))
    (candidates (sort (cl-remove-if-not
                       (lambda (candidate)
                         (string-prefix-p arg candidate t))
                       (hash-table-keys (bah/get-project-markdown-file-names)))
                      (lambda (a b)
                        (let ((count-a (gethash a bah/wikilink-reference-counts 0))
                              (count-b (gethash b bah/wikilink-reference-counts 0)))
                          (if (= count-a count-b)
                              (string-lessp a b)
                            (> count-a count-b))))))
    (sorted t)))

(require 'company-emoji)

(after! markdown-mode
  (set-company-backend! 'markdown-mode 'company-emoji #'bah/wikilinks-backend))
