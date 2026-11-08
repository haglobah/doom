;;; difftastic-parallel.el --- Run difft on a commit's files in parallel -*- lexical-binding: t; -*-

;; git calls its external diff once per file and waits for each, so a
;; commit takes as long as all its difft runs added up.  difftastic's git
;; commands go through bin/difft-par instead: it lets git queue the files,
;; runs difft on them at once, and prints the results in git's order.  The
;; output is the same, so the buffer and its modules see no difference.

(require 'difftastic)

(defvar +difftastic-parallel-script
  (expand-file-name "../bin/difft-par"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The driver that runs git's external diffs in parallel.")

(defun +difftastic-parallel--wrap (args)
  "ARGS of `difftastic--run-command', with a local git command run by the driver."
  (pcase-let ((`(,buffer ,command . ,rest) args))
    (if (and (equal (car command) "git")
             (member "--ext-diff" command)
             (not (file-remote-p default-directory)))
        `(,buffer (,+difftastic-parallel-script "--git" ,@command) ,@rest)
      args)))

(advice-add 'difftastic--run-command :filter-args #'+difftastic-parallel--wrap)

;;; difftastic-parallel.el ends here
