;;; difftastic-groups-test.el --- Tests for difftastic-groups.el -*- lexical-binding: t; -*-
;;; Run via: just test-difftastic

(require 'ert)
(require 'difftastic)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "difftastic-stats.el" dir) nil t)
  (load (expand-file-name "difftastic-groups.el" dir) nil t))

;;; Rules

(ert-deftest difftastic-groups-rules-order-by-review-priority ()
  (let ((view (+difftastic-groups--rule-view
               '(("Justfile" 1 0) ("README.md" 2 0) ("extra.el" 1 1)
                 ("flake.lock" 9 9) ("img.png" nil nil)
                 ("lib/core-test.el" 3 0) ("lib/core.el" 4 4)
                 ("packages.el" 1 0)))))
    (should (eq (plist-get view :source) 'rules))
    (should (equal (mapcar (lambda (g) (cons (plist-get g :key) (plist-get g :files)))
                           (plist-get view :groups))
                   '(("core" "extra.el" "lib/core.el")
                     ("tests" "lib/core-test.el")
                     ("docs" "README.md")
                     ("config" "Justfile")
                     ("deps" "packages.el")
                     ("generated" "flake.lock")
                     ("other" "img.png"))))))

;;; Checking a model's grouping

(defun difftastic-groups-test--group (key paths &rest more)
  "A group alist; MORE entries shadow the defaults."
  `(,@more
    (key . ,key) (label . ,(upcase key)) (summary . "why") (category . "core")
    (critical . nil) (filePaths . ,paths) (fileNotes . nil) (lineNotes . nil)))

(ert-deftest difftastic-groups-normalize-keeps-a-complete-grouping ()
  (let ((view (+difftastic-groups--normalize
               `((overallSummary . "All of it.")
                 (groups . (,(difftastic-groups-test--group
                              "b" '("b.el")
                              '(critical . t)
                              '(lineNotes . (((path . "b.el") (line . 3)
                                              (side . "new") (text . "Careful.")))))
                            ,(difftastic-groups-test--group "a" '("a.el")))))
               '("a.el" "b.el"))))
    (should (eq (plist-get view :source) 'llm))
    (should (equal (plist-get view :summary) "All of it."))
    (should (equal (mapcar (lambda (g) (plist-get g :key)) (plist-get view :groups))
                   '("b" "a")))
    (should (plist-get (car (plist-get view :groups)) :critical))
    (should (equal (plist-get (car (plist-get view :groups)) :line-notes)
                   '((:path "b.el" :line 3 :side "new" :text "Careful."))))
    (should-not (plist-get view :warnings))))

(ert-deftest difftastic-groups-normalize-reports-what-it-repairs ()
  (let* ((view (+difftastic-groups--normalize
                `((overallSummary . "s")
                  (groups . (,(difftastic-groups-test--group
                               "a" '("a.el" "ghost.el")
                               '(fileNotes . (((path . "ghost.el") (text . "x"))
                                              ((path . "a.el") (text . "y")))))
                             ,(difftastic-groups-test--group "a" '("a.el" "b.el"))
                             ,(difftastic-groups-test--group "empty" '("ghost.el")))))
                '("a.el" "b.el" "c.el")))
         (groups (plist-get view :groups)))
    (should (equal (mapcar (lambda (g) (cons (plist-get g :key) (plist-get g :files)))
                           groups)
                   '(("a" "a.el") ("a-2" "b.el") ("ungrouped" "c.el"))))
    (should (equal (plist-get (car groups) :file-notes) '(("a.el" . "y"))))
    (should (equal (plist-get view :warnings)
                   '("Unknown path ghost.el dropped"
                     "a.el was in two groups; kept the first"
                     "1 file left ungrouped by the model")))))

(ert-deftest difftastic-groups-normalize-rejects-a-non-grouping ()
  (should-error (+difftastic-groups--normalize '((foo . 1)) '("a.el"))
                :type '+difftastic-groups-invalid))

;;; What the model sees

(ert-deftest difftastic-groups-number-patch ()
  (should (equal (+difftastic-groups--number-patch
                  (concat "msg -line\n"
                          "diff --git a/x b/x\n--- a/x\n+++ b/x\n"
                          "@@ -10,3 +20,3 @@ ctx\n a\n-b\n+B\n c\n"
                          "\\ No newline at end of file"))
                 (concat "msg -line\n"
                         "diff --git a/x b/x\n--- a/x\n+++ b/x\n"
                         "@@ -10,3 +20,3 @@ ctx\n"
                         "    20| a\n-   11| b\n+   21| B\n    22| c\n"
                         "\\ No newline at end of file"))))

(ert-deftest difftastic-groups-prompt-omits-generated-and-overflowing-diffs ()
  (let* ((+difftastic-groups-patch-limit 200)
         (body (lambda (name n)
                 (concat "diff --git a/" name " b/" name "\n@@ -1 +1 @@\n"
                         (mapconcat (lambda (i) (format "+line %d" i))
                                    (number-sequence 1 n) "\n")
                         "\n")))
         (prompt (+difftastic-groups--prompt
                  "HEAD" "commit"
                  '(("a.el" 3 0) ("flake.lock" 2 0) ("b.el" 40 0))
                  (concat "The message\n\n" (funcall body "a.el" 3)
                          (funcall body "flake.lock" 2) (funcall body "b.el" 40))
                  nil)))
    (should (string-match-p "Change: commit HEAD" prompt))
    (should (string-match-p "^flake.lock  \\+2/-0  \\[generated\\]$" prompt))
    (should (string-match-p "The message" prompt))
    (should (string-match-p "+    3| line 3" prompt))
    (should (string-match-p "(generated file, diff omitted)" prompt))
    (should-not (string-match-p "line 40" prompt))
    (should (string-match-p "(diff omitted: patch too large)" prompt))))

;;; Reading difft's rows

(defmacro difftastic-groups-test--with-chunk (text &rest body)
  "Run BODY in a buffer holding the difft chunk TEXT, with `bounds' bound."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text)
     (let ((bounds (cons (point-min) (point-max))))
       ,@body)))

(defun difftastic-groups-test--numbers (rows)
  (mapcar #'cdr rows))

(ert-deftest difftastic-groups-rows-of-an-added-file ()
  ;; As difft drew difftastic-stats.el in 0d7afba; difftastic's parser
  ;; reads no number from \" 10 \".
  (difftastic-groups-test--with-chunk
      (concat "a.el --- Emacs Lisp\n"
              "  9 (require 'magit-diff)\n"
              " 10 \n"
              " 11 (defun f (x)\n"
              "... \"Parse 2\"\n"
              "100 )\n")
    (should (equal (difftastic-groups-test--numbers
                    (+difftastic-groups--rows bounds 'added))
                   '((nil 9) (nil 10) (nil 11) (nil 11) (nil 100))))
    (should (equal (difftastic-groups-test--numbers
                    (+difftastic-groups--rows bounds 'deleted))
                   '((9 nil) (10 nil) (11 nil) (11 nil) (100 nil))))))

(ert-deftest difftastic-groups-rows-inline ()
  (difftastic-groups-test--with-chunk
      (concat "README.md --- Text\n"
              " 9  9 # Title\n"
              "   10 More.\n"
              "10    Gone.\n"
              "11 11 End 4 5\n")
    (should (equal (difftastic-groups-test--numbers
                    (+difftastic-groups--rows bounds 'inline))
                   '((9 9) (nil 10) (10 nil) (11 11))))))

(ert-deftest difftastic-groups-rows-side-by-side ()
  (difftastic-groups-test--with-chunk
      (concat "core.el --- 1/2 --- Emacs Lisp\n"
              "  3 (defun f2 () 2)              3 (defun f2 () 2)\n"
              "  4 (defun f3 () 3)              4 (defun f3 () 30)\n"
              "                                 5 (defun new () 5)\n"
              "  5 (defun old () 5)                \n")
    (should (equal (difftastic-groups-test--numbers
                    (+difftastic-groups--rows bounds 'side-by-side))
                   '((3 3) (4 4) (nil 5) (5 nil))))))

(ert-deftest difftastic-groups-file-statuses-from-the-patch ()
  (should (equal (+difftastic-groups--statuses
                  (concat "msg\n"
                          "diff --git a/a b/a\nnew file mode 100644\n@@ -0,0 +1 @@\n+x\n"
                          "diff --git a/b b/b\ndeleted file mode 100644\n@@ -1 +0,0 @@\n-x\n"
                          "diff --git a/c b/c\n@@ -1 +1 @@\n-x\n+new file mode\n")
                  '(("a" 1 0) ("b" 0 1) ("c" 1 1)))
                 '(("a" . added) ("b" . deleted)))))

;;; The patch behind a buffer

(ert-deftest difftastic-groups-patch-command-is-stable ()
  ;; A second call once carried the first call's revision too: `mapcan'
  ;; spliced onto a shared constant.
  (dotimes (_ 2)
    (should (equal (+difftastic-groups--patch-command
                    '("git" "--no-pager" "show" "--ext-diff" "abc"))
                   '("--no-pager" "show" "--no-ext-diff" "--no-color"
                     "--format=%B" "abc")))))

;;; Reading claude's answer

(ert-deftest difftastic-groups-parse-claude-output ()
  (should (equal (+difftastic-groups--parse-claude-output
                  "{\"is_error\":false,\"structured_output\":{\"overallSummary\":\"s\",\"groups\":[]}}"
                  0 nil)
                 '((overallSummary . "s") (groups))))
  (should (equal (cadr (should-error (+difftastic-groups--parse-claude-output
                                      "{\"is_error\":true,\"result\":\"Credit balance is too low\"}"
                                      1 nil)))
                 "claude: Credit balance is too low"))
  (should (equal (cadr (should-error (+difftastic-groups--parse-claude-output "" 127 nil)))
                 "claude exited 127 without a result"))
  (should (equal (cadr (should-error (+difftastic-groups--parse-claude-output "" 0 300)))
                 "claude timed out after 300s")))

(ert-deftest difftastic-groups-claude-environment-drops-the-api-key ()
  (let ((process-environment '("ANTHROPIC_API_KEY=k" "HOME=/h"))
        (+difftastic-groups-unset-environment '("ANTHROPIC_API_KEY")))
    (let ((process-environment (+difftastic-groups--claude-environment)))
      (should-not (getenv "ANTHROPIC_API_KEY"))
      (should (equal (getenv "HOME") "/h")))))

;;; Cache

(defun difftastic-groups-test--refuse-coding (&rest _)
  (error "Would ask for a coding system"))

(ert-deftest difftastic-groups-cache-round-trips-without-prompting ()
  (let ((+difftastic-groups-cache-directory (make-temp-file "difftastic-groups" t))
        ;; An interactive Emacs asks for a coding system here, which hangs
        ;; a process sentinel.
        (select-safe-coding-system-function #'difftastic-groups-test--refuse-coding))
    (unwind-protect
        (progn
          (+difftastic-groups--write-cache
           "k" (json-parse-string "{\"overallSummary\":\"a — b −1\",\"groups\":[]}"))
          (should (equal (+difftastic-groups--read-cache "k")
                         '((overallSummary . "a — b −1") (groups))))
          (+difftastic-groups--write-marks "k" '("ä"))
          (should (equal (+difftastic-groups--read-marks "k") '("ä"))))
      (delete-directory +difftastic-groups-cache-directory t))))

;;; End to end: a commit in a scratch repository

(defun difftastic-groups-test--git (&rest args)
  (with-temp-buffer
    (unless (zerop (apply #'call-process "git" nil t nil
                          "-c" "user.name=t" "-c" "user.email=t@t" args))
      (error "git %S failed: %s" args (buffer-string)))))

(defun difftastic-groups-test--write (file content)
  (make-directory (file-name-directory (expand-file-name file)) t)
  (with-temp-file file (insert content)))

(defconst difftastic-groups-test--paths
  '("Justfile" "README.md" "extra.el" "flake.lock" "img.png"
    "lib/core-test.el" "lib/core.el" "lib/new.el"))

(defmacro difftastic-groups-test--with-repo (&rest body)
  "Run BODY in a repo whose HEAD commit touches every kind of file."
  `(let* ((default-directory (file-name-as-directory
                              (make-temp-file "difftastic-groups" t)))
          (+difftastic-groups-cache-directory
           (expand-file-name "cache/" default-directory))
          (+difftastic-groups-auto-min-files nil))
     (unwind-protect
         (progn
           (difftastic-groups-test--git "init" "-q")
           (difftastic-groups-test--write
            "lib/core.el" (mapconcat (lambda (i) (format "(defun f%d () %d)\n" i i))
                                     (number-sequence 0 199) ""))
           (difftastic-groups-test--write "lib/core-test.el" "(ert-deftest a () t)\n")
           (difftastic-groups-test--write "README.md" "# Title\n")
           (difftastic-groups-test--write "Justfile" "test:\n    true\n")
           (difftastic-groups-test--write "flake.lock" "{\"a\": 1}\n")
           (difftastic-groups-test--write "extra.el" "(setq x 1)\n")
           (with-temp-file "img.png" (set-buffer-multibyte nil)
                           (insert (apply #'unibyte-string (number-sequence 0 255))))
           (difftastic-groups-test--git "add" ".")
           (difftastic-groups-test--git "commit" "-qm" "one")
           (difftastic-groups-test--write
            "lib/core.el" (mapconcat (lambda (i) (format "(defun f%d () %d)\n" i
                                                         (if (memq i '(3 150)) (* 10 i) i)))
                                     (number-sequence 0 199) ""))
           (difftastic-groups-test--write "lib/core-test.el" "(ert-deftest a () t)\n(ert-deftest b () t)\n")
           (difftastic-groups-test--write "README.md" "# Title\nMore.\n")
           (difftastic-groups-test--write "Justfile" "test:\n    false\n")
           (difftastic-groups-test--write "flake.lock" "{\"a\": 2}\n")
           (difftastic-groups-test--write "extra.el" "(setq x 2)\n")
           (with-temp-file "img.png" (set-buffer-multibyte nil)
                           (insert (apply #'unibyte-string (number-sequence 255 0 -1))))
           ;; An added file: difft shows one column of padded numbers, and
           ;; its blank lines make difftastic take it for an inline chunk.
           (difftastic-groups-test--write
            "lib/new.el" (mapconcat (lambda (i) (if (cl-oddp i) (format "(defvar v%d %d)\n" i i) "\n"))
                                    (number-sequence 1 120) ""))
           (difftastic-groups-test--git "add" ".")
           (difftastic-groups-test--git "commit" "-qm" "two")
           ,@body)
       (delete-directory default-directory t))))

(defun difftastic-groups-test--run (fn &rest args)
  "Call difftastic FN with ARGS and return its buffer once it has finished."
  (let* ((difftastic-display-buffer-function #'ignore)
         (difftastic-requested-window-width-function (lambda () 100))
         (proc (apply fn args)))
    (while (process-live-p proc)
      (accept-process-output proc 1))
    (accept-process-output nil 0.1)
    (process-buffer proc)))

(defmacro difftastic-groups-test--with-show (&rest body)
  "Run BODY in the buffer of `git show HEAD' in a fresh repo."
  `(difftastic-groups-test--with-repo
    (let ((buf (difftastic-groups-test--run #'difftastic--magit-show "HEAD")))
      (unwind-protect (with-current-buffer buf ,@body)
        (kill-buffer buf)))))

(defun difftastic-groups-test--file-order ()
  "Paths in the order their output appears in the buffer."
  (let (found)
    (dolist (path difftastic-groups-test--paths)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward (concat "^" (regexp-quote path) " --- ") nil t)
          (push (cons (match-beginning 0) path) found))))
    (mapcar #'cdr (sort found (lambda (a b) (< (car a) (car b)))))))

(defun difftastic-groups-test--overlay-strings (prop)
  "Strings of this buffer's group overlays, in position order."
  (mapcar (lambda (ov) (substring-no-properties
                        (or (overlay-get ov 'before-string)
                            (overlay-get ov 'after-string))))
          (sort (seq-filter (lambda (ov) (eq (overlay-get ov '+difftastic-groups) prop))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun difftastic-groups-test--headings ()
  "Texts of this buffer's group headings, in buffer order."
  (let ((pos (point-min)) found)
    (while (setq pos (text-property-not-all pos (point-max) '+difftastic-groups-heading nil))
      (let ((end (or (next-single-property-change pos '+difftastic-groups-heading)
                     (point-max))))
        (push (buffer-substring-no-properties pos end) found)
        (setq pos end)))
    (nreverse found)))

(defun difftastic-groups-test--line-after-note (text)
  "The buffer line whose end carries the note containing TEXT."
  (when-let* ((ov (seq-find (lambda (ov)
                              (and (eq (overlay-get ov '+difftastic-groups) 'note)
                                   (string-match-p (regexp-quote text)
                                                   (or (overlay-get ov 'after-string) ""))))
                            (overlays-in (point-min) (point-max)))))
    (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))

(defun difftastic-groups-test--header ()
  "The header line as text; `format-mode-line' gives \"\" in batch."
  (mapconcat (lambda (part) (if (stringp part) part (eval (cadr part) t)))
             (ensure-list header-line-format) ""))

(defun difftastic-groups-test--analysis ()
  `((overallSummary . "Speeds up core.")
    (groups . (((key . "speed") (label . "Faster core") (summary . "Why it is faster.")
                (category . "core") (critical . t)
                (filePaths . ("lib/core.el" "lib/core-test.el" "lib/new.el"))
                (fileNotes . (((path . "lib/core-test.el") (text . "Covers f1."))))
                (lineNotes . (((path . "lib/core.el") (line . 151) (side . "new")
                               (text . "New value."))
                              ((path . "lib/core.el") (line . 4) (side . "old")
                               (text . "Old value."))
                              ((path . "lib/core.el") (line . 100) (side . "new")
                               (text . "Not shown."))
                              ((path . "lib/core-test.el") (line . 2) (side . "new")
                               (text . "Added test."))
                              ((path . "lib/new.el") (line . 31) (side . "new")
                               (text . "Thirty.")))))
               ((key . "rest") (label . "Everything else") (summary . "Chores.")
                (category . "config") (critical . :false)
                (filePaths . ("Justfile" "README.md" "extra.el" "flake.lock" "img.png"))
                (fileNotes . ()) (lineNotes . ()))))))

(defun difftastic-groups-test--write-cache (analysis)
  "Cache ANALYSIS for the current difftastic buffer's patch."
  (+difftastic-groups--write-cache
   +difftastic-groups--key (difftastic-groups-test--to-json analysis)))

(defun difftastic-groups-test--to-json (value)
  "VALUE (alists and lists) as `json-serialize' input (hash tables and vectors)."
  (cond ((and (consp value) (consp (car value)) (symbolp (caar value)))
         (let ((table (make-hash-table :test #'equal)))
           (pcase-dolist (`(,k . ,v) value)
             (puthash (symbol-name k) (difftastic-groups-test--to-json v) table))
           table))
        ((null value) [])
        ((consp value) (vconcat (mapcar #'difftastic-groups-test--to-json value)))
        (t value)))

(ert-deftest difftastic-groups-rules-regroup-a-commit ()
  (difftastic-groups-test--with-show
   (should (equal (difftastic-groups-test--file-order)
                  '("extra.el" "lib/core.el" "lib/new.el" "lib/core-test.el" "README.md"
                    "Justfile" "flake.lock" "img.png")))
   ;; The commit message stays on top, the overview follows it.
   (goto-char (point-min))
   (should (looking-at "commit "))
   (should (search-forward "Review guide" nil t))
   (should (< (point) (save-excursion (re-search-forward "^extra.el --- "))))
   (let ((headings (difftastic-groups-test--headings)))
     (should (equal (mapcar (lambda (s) (car (split-string s "  [a-z]+ · ")))
                            headings)
                    '("━━ 1/6  Code" "━━ 2/6  Tests" "━━ 3/6  Docs" "━━ 4/6  Config"
                      "━━ 5/6  Generated" "━━ 6/6  Binary")))
     (should (string-prefix-p "━━ 1/6  Code  core · 3 files  +123 −3\n   Source changes"
                              (car headings))))
   ;; Stats still annotate the moved headers.
   (goto-char (point-min))
   (re-search-forward "^lib/core-test.el --- ")
   (should (seq-some (lambda (ov) (equal (substring-no-properties
                                          (or (overlay-get ov 'after-string) ""))
                                         "  +1 −0"))
                     (overlays-in (pos-eol) (pos-eol))))))

(ert-deftest difftastic-groups-rerun-keeps-one-overview ()
  (difftastic-groups-test--with-show
   (let ((difftastic-rerun-requested-window-width-function (lambda () 100))
         (buf (current-buffer)))
     (difftastic-groups-test--run
      (lambda () (difftastic--rerun nil) (get-buffer-process buf)))
     (should (= 1 (count-matches "Review guide" (point-min) (point-max))))
     (should (= 6 (length (difftastic-groups-test--headings)))))))

(ert-deftest difftastic-groups-cached-analysis-orders-and-annotates ()
  (difftastic-groups-test--with-show
   (difftastic-groups-test--write-cache (difftastic-groups-test--analysis))
   (+difftastic-groups-h)
   (should (equal (difftastic-groups-test--file-order)
                  '("lib/core.el" "lib/core-test.el" "lib/new.el" "Justfile" "README.md"
                    "extra.el" "flake.lock" "img.png")))
   (goto-char (point-min))
   (should (search-forward "Speeds up core." nil t))
   (should (string-match-p "⚠ critical"
                           (car (difftastic-groups-test--headings))))
   (should (string-match-p "Why it is faster."
                           (car (difftastic-groups-test--headings))))
   ;; Line notes sit after the row showing that line.
   (should (string-match-p "1500)$" (difftastic-groups-test--line-after-note "New value.")))
   (should (string-match-p "\\`  4 (defun f3 () 3)"
                           (difftastic-groups-test--line-after-note "Old value.")))
   ;; difft's inline layout, for a file with additions only.
   (should (string-match-p "(ert-deftest b () t)"
                           (difftastic-groups-test--line-after-note "Added test.")))
   ;; An added file's single column is the new side, padded numbers too.
   (should (string-match-p "(defvar v31 31)$"
                           (difftastic-groups-test--line-after-note "Thirty.")))
   ;; File notes, and line notes difft didn't show, go under the file header.
   (let ((file-notes (difftastic-groups-test--overlay-strings 'file-note)))
     (should (seq-some (lambda (s) (string-match-p "Covers f1." s)) file-notes))
     (should (seq-some (lambda (s) (string-match-p "line 100: Not shown." s))
                       file-notes)))
   (should (string-match-p "claude" (difftastic-groups-test--header)))))

(ert-deftest difftastic-groups-notes-carry-a-robot ()
  (let ((note (+difftastic-groups--note "Careful.")))
    (should (string-prefix-p "  🤖 Careful." note))
    (should (memq '+difftastic-groups-note
                  (ensure-list (get-text-property (- (length note) 1) 'face note))))))

(ert-deftest difftastic-groups-copying-a-region-includes-notes ()
  (difftastic-groups-test--with-show
   (difftastic-groups-test--write-cache (difftastic-groups-test--analysis))
   (+difftastic-groups-h)
   (goto-char (point-min))
   (re-search-forward "1500)$")
   (let ((text (filter-buffer-substring (pos-bol) (pos-bol 2))))
     (should (string-match-p "1500)\n  🤖 New value.\n\\'" text))
     (should-not (text-properties-at 0 text)))
   ;; Headings are text, plain when copied.
   (goto-char (point-min))
   (let ((beg (search-forward "━━ 1/2" nil t)))
     (re-search-forward "^lib/core.el --- 1/2")
     (let ((text (filter-buffer-substring (- beg 6) (pos-eol))))
       (should (string-match-p "\\`━━ 1/2  Faster core.*\n   Why it is faster.\n\nlib/core.el --- 1/2"
                               text))
       (should-not (text-properties-at 0 text))))))

(ert-deftest difftastic-groups-copy-notes ()
  (difftastic-groups-test--with-show
   (difftastic-groups-test--write-cache (difftastic-groups-test--analysis))
   (+difftastic-groups-h)
   (let ((kill-ring nil))
     ;; On a noted line: that line's notes.
     (goto-char (point-min))
     (re-search-forward "1500)$")
     (+difftastic-groups-copy-notes)
     (should (equal (current-kill 0) "lib/core.el:151: New value.\n"))
     ;; Elsewhere in a file: all of its notes, file notes first.
     (goto-char (point-min))
     (re-search-forward "^lib/core-test.el --- ")
     (+difftastic-groups-copy-notes)
     (should (equal (current-kill 0)
                    "lib/core-test.el: Covers f1.\nlib/core-test.el:2: Added test.\n")))))

(ert-deftest difftastic-groups-step-through-and-mark-reviewed ()
  (difftastic-groups-test--with-show
   (goto-char (point-min))
   (+difftastic-groups-next)
   (should (looking-at "━━ 1/6  Code"))
   (+difftastic-groups-next)
   (should (looking-at "━━ 2/6  Tests"))
   (+difftastic-groups-previous)
   (should (looking-at "━━ 1/6  Code"))
   (should (string-match-p "1/6 Code" (difftastic-groups-test--header)))
   ;; Marking moves on to the next unreviewed group and persists.
   (+difftastic-groups-toggle-reviewed)
   (should (looking-at "━━ 2/6  Tests"))
   (should (string-match-p "✓ reviewed"
                           (car (difftastic-groups-test--headings))))
   (let ((key +difftastic-groups--key))
     (should (equal (+difftastic-groups--read-marks key) '("core"))))
   ;; RET on an overview line jumps to its group.
   (goto-char (point-min))
   (search-forward "Docs")
   (+difftastic-groups-jump)
   (should (looking-at "━━ 3/6  Docs"))))

(defun difftastic-groups-test--visible-p (regexp)
  "Whether the first match of REGEXP shows."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward regexp)
    (not (invisible-p (match-beginning 0)))))

(ert-deftest difftastic-groups-headings-are-text-point-can-reach ()
  (difftastic-groups-test--with-show
   (goto-char (point-min))
   (should (search-forward "━━ 2/6  Tests" nil t))
   (should (equal (get-text-property (point) '+difftastic-groups-section) "tests"))
   ;; The header line names the group of its heading.
   (should (string-match-p "2/6 Tests" (difftastic-groups-test--header)))
   ;; Folding the file before a heading leaves the heading in view.
   (goto-char (point-min))
   (re-search-forward "^lib/new.el --- ")
   (difftastic-hide-chunk)
   (should (difftastic-groups-test--visible-p "^━━ 2/6  Tests"))
   (should (difftastic-groups-test--visible-p "^lib/core-test.el --- "))))

(ert-deftest difftastic-groups-toggle-folds-a-group ()
  (difftastic-groups-test--with-show
   (goto-char (point-min))
   (search-forward "━━ 1/6  Code")
   (+difftastic-groups-toggle-section)
   (should (looking-at "━━ 1/6  Code"))
   (should-not (difftastic-groups-test--visible-p "^extra.el --- "))
   (should-not (difftastic-groups-test--visible-p "^lib/new.el --- "))
   (should-not (difftastic-groups-test--visible-p "Source changes"))
   (should (difftastic-groups-test--visible-p "^━━ 2/6  Tests"))
   ;; The fold survives a render, here from marking another group.
   (search-forward "━━ 2/6")
   (+difftastic-groups-toggle-reviewed)
   (should-not (difftastic-groups-test--visible-p "^extra.el --- "))
   (goto-char (point-min))
   (search-forward "━━ 1/6  Code")
   (+difftastic-groups-toggle-section)
   (should (difftastic-groups-test--visible-p "^extra.el --- "))
   (should (difftastic-groups-test--visible-p "^lib/new.el --- "))))

(ert-deftest difftastic-groups-toggle-folds-the-overview ()
  (difftastic-groups-test--with-show
   (goto-char (point-min))
   (search-forward "Review guide")
   (+difftastic-groups-toggle-section)
   (should (difftastic-groups-test--visible-p "Review guide"))
   (should-not (difftastic-groups-test--visible-p "^  1 .*Code"))
   (should (difftastic-groups-test--visible-p "^━━ 1/6  Code"))
   (+difftastic-groups-toggle-section)
   (should (difftastic-groups-test--visible-p "^  1 .*Code"))
   (goto-char (point-min))
   (should-error (+difftastic-groups-toggle-section) :type 'user-error)))

(ert-deftest difftastic-groups-classify-in-background ()
  (difftastic-groups-test--with-show
   (let* ((script (expand-file-name "fake-claude" default-directory))
          (+difftastic-groups-claude-command (list script))
          (answer (json-serialize
                   `((is_error . :false)
                     (structured_output
                      . ,(difftastic-groups-test--to-json
                          (difftastic-groups-test--analysis)))))))
     (with-temp-file script
       (insert "#!/bin/sh\ncat > \"$0.stdin\"\ncat <<'EOF'\n" answer "\nEOF\n"))
     (set-file-modes script #o755)
     (+difftastic-groups-classify)
     (should (string-match-p "classifying" (difftastic-groups-test--header)))
     (while (gethash +difftastic-groups--key +difftastic-groups--jobs)
       (accept-process-output nil 0.1))
     (should (equal (car (difftastic-groups-test--file-order)) "lib/core.el"))
     (should (eq (plist-get +difftastic-groups--view :source) 'llm))
     ;; The prompt carried the numbered patch.
     (should (string-match-p "1500)" (with-temp-buffer
                                       (insert-file-contents (concat script ".stdin"))
                                       (buffer-string)))))))

(ert-deftest difftastic-groups-classify-failure-is-visible ()
  (difftastic-groups-test--with-show
   (let* ((script (expand-file-name "fake-claude" default-directory))
          (+difftastic-groups-claude-command (list script)))
     (with-temp-file script
       (insert "#!/bin/sh\ncat >/dev/null\necho '{\"is_error\":true,\"result\":\"Not logged in\"}'\nexit 1\n"))
     (set-file-modes script #o755)
     (+difftastic-groups-classify)
     (while (gethash +difftastic-groups--key +difftastic-groups--jobs)
       (accept-process-output nil 0.1))
     (should (eq (plist-get +difftastic-groups--view :source) 'rules))
     (should (string-match-p "claude failed: claude: Not logged in"
                             (difftastic-groups-test--header))))))

(ert-deftest difftastic-groups-auto-classifies-large-commits-only ()
  (difftastic-groups-test--with-repo
   (let ((+difftastic-groups-auto-min-files 4)
         started)
     (cl-letf (((symbol-function '+difftastic-groups--start)
                (lambda (&rest _) (setq started t))))
       (kill-buffer (difftastic-groups-test--run #'difftastic--magit-show "HEAD"))
       (should started)
       (setq started nil)
       (kill-buffer (difftastic-groups-test--run #'difftastic--git-diff-range
                                                 "HEAD~..HEAD" nil '("extra.el" "README.md")))
       (should-not started)))))

;;; difftastic-groups-test.el ends here
