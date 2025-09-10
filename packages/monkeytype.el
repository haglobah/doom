;;; packages/monkeytype.el -*- lexical-binding: t; -*-

(defun bah/monkeytype-load-and-start (arg &optional file-name num-words)
  (interactive "p")
  (when (/= arg 1)
    (setf file-name (read-file-name "Insert name of file with words: "))
    (setf num-words (read-number "Insert number of words you require: " 50)))

  (let ((res '())
        (final-buffer "*Monkeytype-words*")
        (true-num-words (or num-words 50))
        (num-buffer-words nil)
        (indices nil))

    (with-temp-buffer
      (insert-file-contents
       (or file-name
           (expand-file-name
            "english-10k.txt"
            monkeytype-directory)))

      (setq num-buffer-words
            (count-words
             (point-min)
             (point-max)))
      (setq indices
            (sort
             (cl-loop for i from 0 below true-num-words
                      collect
                      (random (- num-buffer-words i)))
             '<))
      (setq res
            (cl-loop repeat true-num-words
                     for idx in indices
                     collect
                     (progn
                       (goto-char (point-min))
                       (forward-word idx)
                       (let ((word-to-return
                              (string-trim
                               (buffer-substring-no-properties
                                (point)
                                (progn (forward-word) (point))))))
                         (kill-word -1)
                         word-to-return)))))

    (with-current-buffer (get-buffer-create final-buffer)
      (erase-buffer)
      (insert (mapconcat 'identity res " ")))
    (switch-to-buffer final-buffer)
    (monkeytype-buffer)))

(use-package! monkeytype
  :init
  (map! :leader
        :prefix ("m" . "monkeytype")
        :desc "Launch monkeytype" :nv "m" #'bah/monkeytype-load-and-start)
  (custom-set-variables
   '(monkeytype-directory (doom-path doom-user-dir "packages" "monkeytype"))))
