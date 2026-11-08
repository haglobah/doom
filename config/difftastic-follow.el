;;; difftastic-follow.el --- Rerun difftastic on rewritten commits -*- lexical-binding: t; -*-

;; A difftastic buffer opened from a magit log names its commit by
;; hash: `*difftastic git show 2ef82781*'.  Amend or rebase that commit
;; and a plain rerun still shows the old one.  `+difftastic-follow-rerun'
;; first swaps every hash in the buffer's commit or range that no branch
;; (nor HEAD) contains for its rewritten successor: the one commit on
;; HEAD or a branch with the same author and author date.  Amend and
;; rebase keep both; `--reset-author' does not.  Symbolic revs (HEAD,
;; main~1) follow by themselves, so they stay as they are.

(require 'difftastic)

(defun +difftastic-follow--git (&rest args)
  "Output of git ARGS, trimmed, or nil when git fails."
  (with-temp-buffer
    (when (eql 0 (apply #'process-file "git" nil '(t nil) nil args))
      (string-trim (buffer-string)))))

(defun +difftastic-follow--reachable-p (hash)
  "Whether HEAD or a branch contains HASH."
  (or (eql 0 (process-file "git" nil nil nil "merge-base" "--is-ancestor" hash "HEAD"))
      (not (string-empty-p
            (or (+difftastic-follow--git "for-each-ref" "--count=1" "--contains" hash
                                         "refs/heads")
                "")))))

(defun +difftastic-follow--successor (hash)
  "The commit on HEAD or a branch that rewrote HASH, in full, or nil."
  (let* ((format "--format=%H %an <%ae> %ad")
         (ident (+difftastic-follow--git "log" "-1" format "--date=raw" hash))
         (key (and ident (substring ident (1+ (string-search " " ident)))))
         (matches
          (and key
               (seq-keep (lambda (line)
                           (and (equal (substring line (1+ (string-search " " line))) key)
                                (substring line 0 (string-search " " line))))
                         (process-lines-ignore-status
                          "git" "log" format "--date=raw" "HEAD" "--branches")))))
    (if (length= matches 1)
        (car matches)
      (message "difftastic: %s is on no branch and has %s; showing it as it was"
               (substring hash 0 (min 8 (length hash)))
               (if matches "several rewritten candidates" "no rewritten successor"))
      nil)))

(defun +difftastic-follow--hash (token)
  "TOKEN's successor, abbreviated like TOKEN, or TOKEN itself."
  (let ((full (+difftastic-follow--git "rev-parse" "--verify" "-q"
                                       (concat token "^{commit}"))))
    (or (and full (string-prefix-p token full)
             (not (+difftastic-follow--reachable-p full))
             (when-let* ((new (+difftastic-follow--successor full)))
               (substring new 0 (length token))))
        token)))

(defun +difftastic-follow--rev (rev-or-range)
  "REV-OR-RANGE with each stale commit hash swapped for its successor."
  (if (stringp rev-or-range)
      (let ((case-fold-search nil))
        (replace-regexp-in-string "\\<[0-9a-f]\\{7,40\\}\\>"
                                  #'+difftastic-follow--hash rev-or-range t t))
    rev-or-range))

(defun +difftastic-follow--update ()
  "Point this buffer's metadata and name at the rewritten commits."
  (let* ((old (alist-get 'rev-or-range difftastic--metadata))
         (new (let ((default-directory (alist-get 'default-directory difftastic--metadata)))
                (+difftastic-follow--rev old))))
    (unless (equal old new)
      (setf (alist-get 'rev-or-range difftastic--metadata) new)
      (setf (alist-get 'git-command difftastic--metadata)
            (mapcar (lambda (arg) (if (equal arg old) new arg))
                    (alist-get 'git-command difftastic--metadata)))
      (rename-buffer (string-replace old new (buffer-name)) t)
      (message "difftastic: following %s to %s" old new))))

(defun +difftastic-follow-rerun ()
  "Rerun difftastic, following amended or rebased commits.
Prefix arguments go to `difftastic-rerun'."
  (interactive nil difftastic-mode)
  (+difftastic-follow--update)
  (call-interactively #'difftastic-rerun))

;;; difftastic-follow.el ends here
