;;; apheleia.el --- lexical-binding: t; -*-

(after! apheleia
  (setf (alist-get 'oxfmt apheleia-formatters)
        '("oxfmt" "--stdin-filepath" filepath))
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'oxfmt))
