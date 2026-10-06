;;; difftastic-stats-test.el --- Tests for difftastic-stats.el -*- lexical-binding: t; -*-
;;; Run via: just test-difftastic

(require 'ert)
(require 'difftastic)
(load (expand-file-name "difftastic-stats.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

;;; Parsing and commands

(ert-deftest difftastic-stats-parse-numstat ()
  (should (equal (+difftastic-stats--parse
                  (concat "2\t1\tm.py\0"
                          "-\t-\timg.png\0"
                          "1\t1\t\0sub/old.txt\0sub/new.txt\0"))
                 '(("m.py" 2 1) ("img.png" nil nil) ("sub/new.txt" 1 1)))))

(ert-deftest difftastic-stats-command-for-diff ()
  (should (equal (+difftastic-stats--command
                  '("git" "--no-pager" "diff" "--ext-diff" "-w" "main..HEAD" "--" "a.el"))
                 '("git" "--no-pager" "diff" "--no-ext-diff" "--numstat" "-z"
                   "-w" "main..HEAD" "--" "a.el"))))

(ert-deftest difftastic-stats-command-for-show ()
  (should (equal (+difftastic-stats--command
                  '("git" "--no-pager" "show" "--ext-diff" "HEAD"))
                 '("git" "--no-pager" "show" "--no-ext-diff" "--numstat" "-z"
                   "--format=" "HEAD"))))

;;; End to end: a commit and a range in a scratch repository

(defun difftastic-stats-test--git (&rest args)
  (with-temp-buffer
    (unless (zerop (apply #'call-process "git" nil t nil
                          "-c" "user.name=t" "-c" "user.email=t@t" args))
      (error "git %S failed: %s" args (buffer-string)))))

(defun difftastic-stats-test--write (file content)
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-temp-file file (insert content)))

(defmacro difftastic-stats-test--with-repo (&rest body)
  "Run BODY in a repo whose HEAD commit adds, edits, renames and deletes."
  `(let* ((default-directory (file-name-as-directory
                              (make-temp-file "difftastic-stats" t))))
     (unwind-protect
         (progn
           (difftastic-stats-test--git "init" "-q")
           (difftastic-stats-test--write "sub/old.txt" "a\nb\nc\nd\ne\nf\n")
           (difftastic-stats-test--write "m.py" "x = 1\n")
           (difftastic-stats-test--write "del.txt" "gone\n")
           (difftastic-stats-test--git "add" ".")
           (difftastic-stats-test--git "commit" "-qm" "one")
           (difftastic-stats-test--git "mv" "sub/old.txt" "sub/new.txt")
           (difftastic-stats-test--write "sub/new.txt" "a\nB\nc\nd\ne\nf\n")
           (difftastic-stats-test--write "m.py" "x = 2\ny = 3\n")
           (difftastic-stats-test--git "rm" "-q" "del.txt")
           (difftastic-stats-test--write "add.txt" "n\n")
           (difftastic-stats-test--git "add" ".")
           (difftastic-stats-test--git "commit" "-qm" "two")
           ,@body)
       (delete-directory default-directory t))))

(defun difftastic-stats-test--run (fn &rest args)
  "Call difftastic FN with ARGS and return its buffer once it has finished."
  (let* ((difftastic-display-buffer-function #'ignore)
         (difftastic-requested-window-width-function (lambda () 100))
         (proc (apply fn args)))
    (while (process-live-p proc)
      (accept-process-output proc 1))
    ;; Let the sentinel run.
    (accept-process-output nil 0.1)
    (process-buffer proc)))

(defun difftastic-stats-test--header-counts (buffer file)
  "The count shown after each header line of FILE in BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (found)
        (while (re-search-forward (concat "^" (regexp-quote file) " --- ") nil t)
          (push (mapconcat (lambda (ov) (substring-no-properties
                                         (overlay-get ov 'after-string)))
                           (seq-filter (lambda (ov) (overlay-get ov '+difftastic-stats))
                                       (overlays-in (pos-eol) (pos-eol)))
                           "")
                found))
        (nreverse found)))))

(defun difftastic-stats-test--header-line (buffer)
  (with-current-buffer buffer
    (substring-no-properties header-line-format)))

(ert-deftest difftastic-stats-commit-view ()
  (difftastic-stats-test--with-repo
   (let ((buf (difftastic-stats-test--run #'difftastic--magit-show "HEAD")))
     (unwind-protect
         (progn
           (should (equal (difftastic-stats-test--header-counts buf "m.py")
                          '("  +2 −1")))
           (should (equal (difftastic-stats-test--header-counts buf "sub/new.txt")
                          '("  +1 −1")))
           (should (equal (difftastic-stats-test--header-counts buf "del.txt")
                          '("  +0 −1")))
           (should (equal (difftastic-stats-test--header-line buf)
                          " HEAD  4 files  +4 −3")))
       (kill-buffer buf)))))

(ert-deftest difftastic-stats-range-view ()
  (difftastic-stats-test--with-repo
   (let ((buf (difftastic-stats-test--run #'difftastic--git-diff-range
                                          "HEAD~..HEAD" nil '("m.py"))))
     (unwind-protect
         (progn
           (should (equal (difftastic-stats-test--header-counts buf "m.py")
                          '("  +2 −1")))
           (should (equal (difftastic-stats-test--header-line buf)
                          " HEAD~..HEAD  1 file  +2 −1")))
       (kill-buffer buf)))))

(ert-deftest difftastic-stats-rerun-keeps-one-count ()
  (difftastic-stats-test--with-repo
   (let ((buf (difftastic-stats-test--run #'difftastic--magit-show "HEAD")))
     (unwind-protect
         (with-current-buffer buf
           (let ((difftastic-rerun-requested-window-width-function (lambda () 100)))
             (difftastic-stats-test--run
              (lambda () (difftastic--rerun nil) (get-buffer-process buf))))
           (should (equal (difftastic-stats-test--header-counts buf "m.py")
                          '("  +2 −1")))
           (should (equal (difftastic-stats-test--header-line buf)
                          " HEAD  4 files  +4 −3")))
       (kill-buffer buf)))))

(ert-deftest difftastic-stats-git-failure-is-visible ()
  (with-temp-buffer
    ;; Not a repository, so git fails.
    (let ((default-directory (make-temp-file "difftastic-stats" t)))
      (setq difftastic--metadata
            `((default-directory . ,default-directory)
              (rev-or-range . "HEAD")
              (git-command . ("git" "--no-pager" "show" "--ext-diff" "HEAD"))))
      (unwind-protect
          (progn
            (+difftastic-stats-h)
            (should (string-match-p "line counts unavailable"
                                    header-line-format)))
        (delete-directory default-directory t)))))

;;; difftastic-stats-test.el ends here
