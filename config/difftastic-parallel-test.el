;;; difftastic-parallel-test.el --- Tests for difftastic-parallel.el -*- lexical-binding: t; -*-
;;; Run via: just test-difftastic

(require 'ert)
(require 'difftastic)
(load (expand-file-name "difftastic-parallel.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst difftastic-parallel-test--difft
  "difft --color\\=always --width\\=100 --background\\=dark"
  "GIT_EXTERNAL_DIFF as difftastic builds it.")

(defun difftastic-parallel-test--git (&rest args)
  "Output of git ARGS in `default-directory'; error when git fails."
  (with-temp-buffer
    (unless (eql 0 (apply #'process-file "git" nil t nil args))
      (error "git %S failed: %s" args (buffer-string)))
    (buffer-string)))

(defun difftastic-parallel-test--write (file text)
  "Write TEXT to FILE, creating its directory."
  (make-directory (or (file-name-directory file) ".") t)
  (with-temp-file file (insert text)))

(defun difftastic-parallel-test--run (ext program &rest args)
  "Run PROGRAM ARGS with GIT_EXTERNAL_DIFF=EXT: (STATUS STDOUT STDERR)."
  (let ((process-environment (cons (concat "GIT_EXTERNAL_DIFF=" ext)
                                   process-environment))
        (err (make-temp-file "difft-par-err")))
    (unwind-protect
        (with-temp-buffer
          (let ((status (apply #'process-file program nil (list t err) nil args)))
            (list status (buffer-string)
                  (with-temp-buffer (insert-file-contents err) (buffer-string)))))
      (delete-file err))))

(defun difftastic-parallel-test--serial (ext &rest git-args)
  "Plain git GIT-ARGS with EXT as external diff: (STATUS STDOUT STDERR)."
  (apply #'difftastic-parallel-test--run ext "git" git-args))

(defun difftastic-parallel-test--parallel (ext &rest git-args)
  "git GIT-ARGS through the parallel driver: (STATUS STDOUT STDERR)."
  (apply #'difftastic-parallel-test--run ext +difftastic-parallel-script
         "--git" "git" git-args))

(defmacro difftastic-parallel-test--with-repo (&rest body)
  "Run BODY in a fresh git repo whose HEAD touches files in many ways."
  (declare (indent 0))
  `(let* ((default-directory (file-name-as-directory (make-temp-file "difft-par" t)))
          (process-environment
           (append '("GIT_AUTHOR_NAME=A" "GIT_AUTHOR_EMAIL=a@x"
                     "GIT_COMMITTER_NAME=A" "GIT_COMMITTER_EMAIL=a@x"
                     "GIT_AUTHOR_DATE=1700000000 +0000"
                     "GIT_COMMITTER_DATE=1700000000 +0000"
                     "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_NOSYSTEM=1")
                   process-environment)))
     (unwind-protect
         (progn
           (difftastic-parallel-test--git "init" "-q" "-b" "main")
           (difftastic-parallel-test--write "src/a.ts" "const a = 1;\nexport function f(x: number) {\n  return x + a;\n}\n")
           (difftastic-parallel-test--write "src/b.el" "(defun b (x)\n  (+ x 1))\n")
           (difftastic-parallel-test--write "gone.txt" "bye\n")
           (difftastic-parallel-test--write "old-name.py" "def g(y):\n    return y * 2\n\n\nprint(g(3))\n")
           (difftastic-parallel-test--write "run.sh" "echo hi\n")
           (difftastic-parallel-test--write "blob.bin" "\0\1\2")
           (difftastic-parallel-test--git "add" "-A")
           (difftastic-parallel-test--git "commit" "-q" "-m" "first")
           (difftastic-parallel-test--write "src/a.ts" "const a = 2;\nexport function f(x: number, y = 0) {\n  return x + a + y;\n}\n")
           (difftastic-parallel-test--write "src/b.el" "(defun b (x y)\n  (+ x y 1))\n")
           (delete-file "gone.txt")
           (difftastic-parallel-test--git "mv" "old-name.py" "new-name.py")
           (difftastic-parallel-test--write "new-name.py" "def g(y):\n    return y * 3\n\n\nprint(g(3))\n")
           (set-file-modes "run.sh" #o755)
           (difftastic-parallel-test--write "blob.bin" "\0\1\3")
           (difftastic-parallel-test--write "added.md" "# New\n\nText.\n")
           (difftastic-parallel-test--git "add" "-A")
           (difftastic-parallel-test--git "commit" "-q" "-m" "second\n\nWith a body.")
           ,@body)
       (delete-directory default-directory t))))

;;; Same output as serial git

(ert-deftest difftastic-parallel-show-matches-serial ()
  (difftastic-parallel-test--with-repo
    (let ((serial (difftastic-parallel-test--serial
                   difftastic-parallel-test--difft "--no-pager" "show" "--ext-diff" "HEAD")))
      (should (eql 0 (car serial)))
      (should (string-match-p "new-name.py" (nth 1 serial)))
      (should (equal (difftastic-parallel-test--parallel
                      difftastic-parallel-test--difft "--no-pager" "show" "--ext-diff" "HEAD")
                     serial)))))

(ert-deftest difftastic-parallel-range-matches-serial ()
  (difftastic-parallel-test--with-repo
    (let ((args '("--no-pager" "diff" "--ext-diff" "HEAD~1..HEAD")))
      (should (equal (apply #'difftastic-parallel-test--parallel
                            difftastic-parallel-test--difft args)
                     (apply #'difftastic-parallel-test--serial
                            difftastic-parallel-test--difft args))))))

(ert-deftest difftastic-parallel-worktree-and-staged-match-serial ()
  (difftastic-parallel-test--with-repo
    (difftastic-parallel-test--write "src/a.ts" "const a = 3;\n")
    (difftastic-parallel-test--write "src/b.el" "(defun b ())\n")
    (difftastic-parallel-test--git "add" "src/b.el")
    (dolist (args '(("--no-pager" "diff" "--ext-diff")
                    ("--no-pager" "diff" "--ext-diff" "--cached")
                    ("--no-pager" "diff" "--ext-diff" "HEAD")))
      (let ((serial (apply #'difftastic-parallel-test--serial
                           difftastic-parallel-test--difft args)))
        (should (string-match-p "a.ts\\|b.el" (nth 1 serial)))
        (should (equal (apply #'difftastic-parallel-test--parallel
                              difftastic-parallel-test--difft args)
                       serial))))))

(ert-deftest difftastic-parallel-subdirectory-matches-serial ()
  (difftastic-parallel-test--with-repo
    (difftastic-parallel-test--write "src/a.ts" "const a = 3;\n")
    (let ((default-directory (expand-file-name "src/")))
      (dolist (args '(("--no-pager" "diff" "--ext-diff")
                      ("--no-pager" "show" "--ext-diff" "HEAD")))
        (should (equal (apply #'difftastic-parallel-test--parallel
                              difftastic-parallel-test--difft args)
                       (apply #'difftastic-parallel-test--serial
                              difftastic-parallel-test--difft args)))))))

;;; Parallel runs, git's order

(defun difftastic-parallel-test--slow-diff (dir)
  "A fake external diff in DIR: sleeps 0.6s for a.ts, 0.1s else, prints its path."
  (let ((script (expand-file-name "slow-diff" dir)))
    (difftastic-parallel-test--write
     script "#!/bin/sh\ncase \"$1\" in *a.ts) sleep 0.6 ;; *) sleep 0.1 ;; esac\necho \"diff $1\"\n")
    (set-file-modes script #o755)
    script))

(ert-deftest difftastic-parallel-runs-diffs-concurrently-in-git-order ()
  (difftastic-parallel-test--with-repo
    (let* ((tmp (make-temp-file "difft-par-bin" t))
           (ext (difftastic-parallel-test--slow-diff tmp))
           (args '("--no-pager" "show" "--ext-diff" "--format=" "HEAD"))
           (serial (apply #'difftastic-parallel-test--serial ext args))
           (start (float-time))
           (parallel (apply #'difftastic-parallel-test--parallel ext args))
           (elapsed (- (float-time) start)))
      (delete-directory tmp t)
      (should (equal parallel serial))
      ;; Serial: 0.6 + 6 × 0.1 = 1.2s.
      (should (< elapsed 1.0)))))

;;; Failures stay visible

(ert-deftest difftastic-parallel-reports-a-failing-diff ()
  (difftastic-parallel-test--with-repo
    (let* ((tmp (make-temp-file "difft-par-bin" t))
           (ext (expand-file-name "fail-diff" tmp)))
      (difftastic-parallel-test--write
       ext "#!/bin/sh\necho \"diff $1\"\ncase \"$1\" in *b.el) echo boom >&2; exit 3 ;; esac\n")
      (set-file-modes ext #o755)
      (pcase-let ((`(,status ,out ,err)
                   (difftastic-parallel-test--parallel
                    ext "--no-pager" "show" "--ext-diff" "--format=" "HEAD")))
        (delete-directory tmp t)
        (should-not (eql 0 status))
        ;; The other files still show.
        (should (string-match-p "diff src/a.ts" out))
        (should (string-match-p "boom" err))
        (should (string-match-p "src/b.el" err))))))

(ert-deftest difftastic-parallel-passes-on-git-failure ()
  (difftastic-parallel-test--with-repo
    (pcase-let ((`(,status ,_ ,err)
                 (difftastic-parallel-test--parallel
                  difftastic-parallel-test--difft "--no-pager" "show" "--ext-diff" "no-such-rev")))
      (should (eql 128 status))
      (should (string-match-p "no-such-rev" err)))))

(ert-deftest difftastic-parallel-leaves-no-temp-files ()
  (difftastic-parallel-test--with-repo
    (let* ((tmp (file-name-as-directory (make-temp-file "difft-par-tmp" t)))
           (process-environment (cons (concat "TMPDIR=" tmp) process-environment)))
      (difftastic-parallel-test--parallel
       difftastic-parallel-test--difft "--no-pager" "show" "--ext-diff" "HEAD")
      (should (null (directory-files tmp nil directory-files-no-dot-files-regexp)))
      (delete-directory tmp t))))

;;; Wrapping difftastic's commands

(ert-deftest difftastic-parallel-wraps-local-git-ext-diff-commands ()
  (let ((default-directory "/tmp/"))
    (should (equal (+difftastic-parallel--wrap
                    '(buf ("git" "--no-pager" "show" "--ext-diff" "HEAD") act))
                   `(buf (,+difftastic-parallel-script "--git"
                          "git" "--no-pager" "show" "--ext-diff" "HEAD")
                         act)))
    ;; difft run on files directly has nothing to parallelize.
    (should (equal (+difftastic-parallel--wrap '(buf ("difft" "a" "b")))
                   '(buf ("difft" "a" "b"))))))

(ert-deftest difftastic-parallel-leaves-remote-commands-alone ()
  (let ((default-directory "/ssh:host:/tmp/")
        (command '("git" "--no-pager" "show" "--ext-diff" "HEAD")))
    (should (equal (+difftastic-parallel--wrap (list 'buf command))
                   (list 'buf command)))))

;;; difftastic-parallel-test.el ends here
