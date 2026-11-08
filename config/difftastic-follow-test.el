;;; difftastic-follow-test.el --- Tests for difftastic-follow.el -*- lexical-binding: t; -*-
;;; Run via: just test-difftastic

(require 'ert)
(require 'difftastic)
(load (expand-file-name "difftastic-follow.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defun difftastic-follow-test--git (&rest args)
  "Output of git ARGS in `default-directory', trimmed."
  (with-temp-buffer
    (unless (eql 0 (apply #'process-file "git" nil t nil args))
      (error "git %S failed: %s" args (buffer-string)))
    (string-trim (buffer-string))))

(defun difftastic-follow-test--commit (file text message &rest extra)
  "Write TEXT to FILE and commit it with MESSAGE and EXTRA args; the hash."
  (with-temp-file file (insert text))
  (difftastic-follow-test--git "add" file)
  (apply #'difftastic-follow-test--git "commit" "-q" "-m" message extra)
  (difftastic-follow-test--git "rev-parse" "HEAD"))

(defmacro difftastic-follow-test--with-repo (&rest body)
  "Run BODY in a fresh git repo with two commits."
  (declare (indent 0))
  `(let* ((default-directory (file-name-as-directory (make-temp-file "follow" t)))
          (process-environment
           (append '("GIT_AUTHOR_NAME=A" "GIT_AUTHOR_EMAIL=a@x"
                     "GIT_COMMITTER_NAME=A" "GIT_COMMITTER_EMAIL=a@x"
                     "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment)))
     (unwind-protect
         (progn
           (difftastic-follow-test--git "init" "-q" "-b" "main")
           (let ((process-environment
                  (cons "GIT_AUTHOR_DATE=1700000000 +0000" process-environment)))
             (difftastic-follow-test--commit "a" "1" "first"))
           (let ((process-environment
                  (cons "GIT_AUTHOR_DATE=1700000100 +0000" process-environment)))
             (difftastic-follow-test--commit "a" "2" "second"))
           ,@body)
       (delete-directory default-directory t))))

(defun difftastic-follow-test--amend ()
  "Amend HEAD with a new change; the new hash."
  (difftastic-follow-test--commit "b" "x" "second, amended" "--amend"))

;;; Following a rev

(ert-deftest difftastic-follow-keeps-a-reachable-hash ()
  (difftastic-follow-test--with-repo
    (let ((head (substring (difftastic-follow-test--git "rev-parse" "HEAD") 0 8)))
      (should (equal (+difftastic-follow--rev head) head)))))

(ert-deftest difftastic-follow-keeps-symbolic-revs ()
  (difftastic-follow-test--with-repo
    (should (equal (+difftastic-follow--rev "HEAD") "HEAD"))
    (should (equal (+difftastic-follow--rev "main~1..main") "main~1..main"))
    (should (eq (+difftastic-follow--rev 'unstaged) 'unstaged))))

(ert-deftest difftastic-follow-finds-the-amended-commit ()
  (difftastic-follow-test--with-repo
    (let* ((old (substring (difftastic-follow-test--git "rev-parse" "HEAD") 0 8))
           (new (difftastic-follow-test--amend)))
      (should (equal (+difftastic-follow--rev old) (substring new 0 8))))))

(ert-deftest difftastic-follow-follows-each-end-of-a-range ()
  (difftastic-follow-test--with-repo
    (let* ((base (substring (difftastic-follow-test--git "rev-parse" "HEAD~1") 0 7))
           (old (substring (difftastic-follow-test--git "rev-parse" "HEAD") 0 7))
           (new (substring (difftastic-follow-test--amend) 0 7)))
      (should (equal (+difftastic-follow--rev (concat base ".." old))
                     (concat base ".." new)))
      (should (equal (+difftastic-follow--rev (concat base "..." old "^"))
                     (concat base "..." new "^"))))))

(ert-deftest difftastic-follow-keeps-a-hash-without-a-successor ()
  (difftastic-follow-test--with-repo
    (let ((old (difftastic-follow-test--git "rev-parse" "HEAD")))
      (difftastic-follow-test--git "reset" "-q" "--hard" "HEAD~1")
      (should (equal (+difftastic-follow--rev old) old)))))

;;; Updating the buffer

(ert-deftest difftastic-follow-updates-metadata-and-buffer-name ()
  (difftastic-follow-test--with-repo
    (let* ((old (substring (difftastic-follow-test--git "rev-parse" "HEAD") 0 8))
           (new (substring (difftastic-follow-test--amend) 0 8))
           (buf (get-buffer-create (format "*difftastic git show %s*" old))))
      (unwind-protect
          (with-current-buffer buf
            (setq difftastic--metadata
                  `((default-directory . ,default-directory)
                    (rev-or-range . ,old)
                    (git-command . ("git" "--no-pager" "show" "--ext-diff" ,old))))
            (+difftastic-follow--update)
            (should (equal (alist-get 'rev-or-range difftastic--metadata) new))
            (should (equal (alist-get 'git-command difftastic--metadata)
                           `("git" "--no-pager" "show" "--ext-diff" ,new)))
            (should (equal (buffer-name) (format "*difftastic git show %s*" new))))
        (kill-buffer buf)))))

;;; difftastic-follow-test.el ends here
