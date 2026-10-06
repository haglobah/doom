;;; difftastic-groups.el --- Review difftastic diffs group by group -*- lexical-binding: t; -*-

;; Groups the files of a difftastic buffer by intent and orders the
;; groups by review priority, after antfu's pulls.review (MIT; the
;; grouping prompt below adapts its prompt.ts).
;;
;; Path rules group the files at once: code, tests, docs, config, deps,
;; generated, binary.  `claude -p' can classify the diff instead: it
;; groups by intent, orders the groups core change first, and adds
;; summaries and review notes.  It runs in the background, by itself for
;; a commit or a range of `+difftastic-groups-auto-min-files' or more
;; files, and the buffer regroups when it answers.  Results are cached
;; by the patch's hash, so each commit is classified once.
;;
;; The buffer gets an overview after the commit message, a heading above
;; each group, and notes under file headers and changed lines.  The
;; header line shows the group at point and where the grouping came from.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'magit-section)                ; magit-section-heading face
(require 'difftastic)
;; Also needs difftastic-stats.el: its numstat entries are the file list.

;;; Settings

(defvar +difftastic-groups-auto-min-files 4
  "Classify a commit or range by itself when it changes this many files.
nil classifies only on `+difftastic-groups-classify'.")

(defvar +difftastic-groups-claude-command '("claude")
  "Program, and leading arguments, that run Claude Code.")

(defvar +difftastic-groups-model "claude-sonnet-5-5"
  "Model claude classifies with.")

(defvar +difftastic-groups-unset-environment '("ANTHROPIC_API_KEY")
  "Variables removed from claude's environment.
Without an API key, claude uses its own login.")

(defvar +difftastic-groups-timeout 300
  "Seconds before a classification is given up.")

(defvar +difftastic-groups-patch-limit 150000
  "Characters of patch sent to claude.
Files past the limit are sent by path and line counts only.")

(defvar +difftastic-groups-cache-directory
  (expand-file-name "difftastic-groups/"
                    (if (boundp 'doom-cache-dir)
                        doom-cache-dir
                      (locate-user-emacs-file "cache/")))
  "Where classifications and review marks are kept, by patch hash.")

(defconst +difftastic-groups--version 1
  "Bump when the prompt or schema changes, so cached results are redone.")

(defface +difftastic-groups-heading
  '((t :inherit magit-section-heading :overline t :extend t))
  "Group headings and the overview title."
  :group 'difftastic)

(defface +difftastic-groups-summary '((t :inherit font-lock-doc-face))
  "Summaries under group headings and in the overview."
  :group 'difftastic)

(defface +difftastic-groups-note '((t :foreground "#d7a26e" :slant italic))
  "Claude's notes under files and lines; `+difftastic-groups-set-faces'
blends the theme's orange into the text color."
  :group 'difftastic)

(defun +difftastic-groups--theme-orange ()
  "The current theme's orange, or nil: doom themes' orange, catppuccin's peach."
  (or (and (fboundp 'doom-color) (doom-color 'orange))
      (and (fboundp 'catppuccin-get-color)
           (memq 'catppuccin custom-enabled-themes)
           (catppuccin-get-color 'peach))))

(defun +difftastic-groups-set-faces (&rest _)
  "Make notes slightly orange: the theme's orange, blended into its text."
  (when-let* ((orange (+difftastic-groups--theme-orange))
              ((fboundp 'doom-blend)))
    (set-face-attribute '+difftastic-groups-note nil
                        :foreground (doom-blend orange
                                                (face-foreground 'default nil t)
                                                0.6))))
(+difftastic-groups-set-faces)
(add-hook 'doom-load-theme-hook #'+difftastic-groups-set-faces)

(define-error '+difftastic-groups-invalid "Not a grouping")

;;; Buffer state

(defvar-local +difftastic-groups--key nil "Hash of this buffer's patch.")
(defvar-local +difftastic-groups--patch nil "This buffer's plain patch.")
(defvar-local +difftastic-groups--view nil "The grouping shown.")
(defvar-local +difftastic-groups--steps nil "(GROUP . MARKER) per shown group.")
(defvar-local +difftastic-groups--marks nil "Keys of groups marked reviewed.")
(defvar-local +difftastic-groups--file-statuses nil
  "Alist of PATH to `added' or `deleted', for this buffer's diff.")

;;; Path rules

(defconst +difftastic-groups--rules
  '(("core" "Code" "Source changes: the core of the diff.")
    ("tests" "Tests" "Tests and fixtures."
     "\\(?:\\`\\|/\\)\\(?:tests?\\|__tests__\\|spec\\)/"
     "[-_.]\\(?:test\\|spec\\)\\.[^/.]+\\'"
     "\\(?:\\`\\|/\\)test_[^/]*\\.py\\'")
    ("docs" "Docs" "Documentation."
     "\\.\\(?:md\\|mdx\\|org\\|rst\\|adoc\\)\\'"
     "\\(?:\\`\\|/\\)docs?/"
     "\\(?:\\`\\|/\\)\\(?:README\\|CHANGELOG\\|LICENSE\\)[^/]*\\'")
    ("config" "Config" "Build, CI and tool configuration."
     "\\(?:\\`\\|/\\)\\.github/"
     "\\(?:\\`\\|/\\)\\(?:[Jj]ustfile\\|Makefile\\|Dockerfile\\|docker-compose[^/]*\\.ya?ml\\|\\.envrc\\|\\.editorconfig\\|tsconfig[^/]*\\.json\\)\\'"
     "\\(?:\\`\\|/\\)\\.[^/]*rc\\(?:\\.[^/]*\\)?\\'"
     "\\(?:\\`\\|/\\)\\.git[^/]*\\'"
     "\\.config\\.[^/]+\\'")
    ("deps" "Dependencies" "Declared dependencies."
     "\\(?:\\`\\|/\\)\\(?:package\\.json\\|Cargo\\.toml\\|go\\.mod\\|pyproject\\.toml\\|requirements[^/]*\\.txt\\|Gemfile\\|packages\\.el\\|flake\\.nix\\|deps\\.edn\\|[^/]*\\.cabal\\)\\'")
    ("generated" "Generated" "Lockfiles and build output; rarely worth reading line by line."
     "\\(?:\\`\\|/\\)\\(?:[^/]*\\.lock\\|package-lock\\.json\\|pnpm-lock\\.yaml\\|go\\.sum\\)\\'"
     "\\(?:\\`\\|/\\)dist/"
     "\\.generated\\.\\|\\.min\\.\\(?:js\\|css\\)\\'")
    ("other" "Binary" "Binary files."))
  "(KEY LABEL SUMMARY . REGEXPS) in review order.")

(defconst +difftastic-groups--rule-match-order
  '("generated" "tests" "docs" "deps" "config")
  "Rules a path is tried against, first match wins; else code.")

(defun +difftastic-groups--rule-key (path binary)
  "The key of the rule for PATH; BINARY files are \"other\"."
  (if binary
      "other"
    (or (seq-find (lambda (key)
                    (seq-some (lambda (re) (string-match-p re path))
                              (nthcdr 3 (assoc key +difftastic-groups--rules))))
                  +difftastic-groups--rule-match-order)
        "core")))

(defun +difftastic-groups--generated-p (path)
  "Non-nil when PATH looks machine-generated."
  (equal (+difftastic-groups--rule-key path nil) "generated"))

(defun +difftastic-groups--rule-view (entries)
  "Group stats ENTRIES, (PATH ADDED DELETED), by path rules."
  (let ((files (make-hash-table :test #'equal)))
    (pcase-dolist (`(,path ,added . ,_) entries)
      (push path (gethash (+difftastic-groups--rule-key path (null added)) files)))
    (list :source 'rules
          :groups
          (delq nil
                (mapcar (pcase-lambda (`(,key ,label ,summary . ,_))
                          (when-let* ((paths (gethash key files)))
                            (list :key key :label label :summary summary
                                  :category key :files (nreverse paths))))
                        +difftastic-groups--rules)))))

;;; Checking a model's grouping

(defun +difftastic-groups--normalize (analysis paths)
  "Check ANALYSIS, claude's parsed answer, against the diff's PATHS.
Return a view like `+difftastic-groups--rule-view' does.  Repairs are
listed under :warnings: unknown paths are dropped, a path in two groups
stays in the first, and paths no group took form an \"Ungrouped\" group."
  (let ((groups (alist-get 'groups analysis))
        (known (make-hash-table :test #'equal))
        (taken (make-hash-table :test #'equal))
        (keys (make-hash-table :test #'equal))
        warnings result)
    (unless (and (consp analysis) (listp groups) (assq 'groups analysis))
      (signal '+difftastic-groups-invalid (list analysis)))
    (dolist (path paths) (puthash path t known))
    (let ((path-ok (lambda (path)
                     (or (gethash path known)
                         (progn (cl-pushnew (format "Unknown path %s dropped" path)
                                            warnings :test #'equal)
                                nil)))))
      (dolist (group groups)
        (let-alist group
          (let (files)
            (dolist (path .filePaths)
              (cond ((not (funcall path-ok path)))
                    ((gethash path taken)
                     (push (format "%s was in two groups; kept the first" path) warnings))
                    (t (puthash path t taken) (push path files))))
            (when files
              (let ((key (or .key "group")))
                (puthash key (1+ (gethash key keys 0)) keys)
                (when (> (gethash key keys) 1)
                  (setq key (format "%s-%d" key (gethash key keys))))
                (push (list :key key
                            :label (or .label key)
                            :summary .summary
                            :category (or .category "other")
                            :critical (eq .critical t)
                            :files (nreverse files)
                            :file-notes
                            (cl-loop for note in .fileNotes
                                     for path = (alist-get 'path note)
                                     when (funcall path-ok path)
                                     collect (cons path (alist-get 'text note)))
                            :line-notes
                            (cl-loop for note in .lineNotes
                                     for path = (alist-get 'path note)
                                     when (and (funcall path-ok path)
                                               (integerp (alist-get 'line note)))
                                     collect (list :path path
                                                   :line (alist-get 'line note)
                                                   :side (alist-get 'side note)
                                                   :text (alist-get 'text note))))
                      result)))))))
    (when-let* ((left (seq-remove (lambda (p) (gethash p taken)) paths)))
      (push (format "%d file%s left ungrouped by the model"
                    (length left) (if (cdr left) "s" ""))
            warnings)
      (push (list :key "ungrouped" :label "Ungrouped" :category "other"
                  :summary "Files the model did not place in any group."
                  :files left)
            result))
    (list :source 'llm
          :summary (alist-get 'overallSummary analysis)
          :groups (nreverse result)
          :warnings (nreverse warnings))))

;;; What claude sees

(defconst +difftastic-groups--system-prompt
  "<role>
You organize the changed files of a git commit or range into review groups, so a reviewer can read the change intent by intent, in order of importance, instead of file by file.
</role>

<grouping_principles>
- Group by intent, not by directory. One feature touching several modules is ONE group; split it only when the parts can be reviewed independently.
- Use the fewest groups that still separate independent intents. A typical change has 1-5 groups. A single-file group needs a reason.
- Tests, stories and fixtures belong to the feature they cover. Only tests unrelated to any feature form their own group.
- Order groups by review priority: the core change first, supporting changes next, mechanical changes (lockfiles, generated files, formatting, renames) last.
- Every path in the manifest goes into exactly one group.
- Commit messages, when given, hint at the author's intents. Group by the final change, not by commit: fixup and WIP commits often mix concerns.
</grouping_principles>

<output>
- \"summary\" and \"overallSummary\" explain why over what, in 1-3 sentences of plain text. Backticks around identifiers are fine; no other Markdown.
- \"key\" is short, stable kebab-case. \"label\" is at most 4 words.
- \"fileNotes\" and \"lineNotes\" are sparing. Add a note only where it saves the reviewer time - non-obvious logic, a subtle behavior change, a risk - never to explain the obvious. Most files need none. A note is at most 2 sentences.
- Patch lines carry their line number after the marker: '-' lines the old file's number (side \"old\"), '+' and ' ' lines the new file's (side \"new\"). A line note names one such line, preferably a changed one, and only lines shown in the patch.
- Set \"critical\" only for a group that deserves extra care: security, data loss, hard to revert. Most groups are not critical.
</output>"
  "System prompt for classifying a diff.")

(defun +difftastic-groups--schema ()
  "JSON schema of claude's answer, as a string."
  (cl-labels ((obj (&rest props)
                `(:type "object" :additionalProperties :false
                  :required ,(vconcat (cl-loop for (k _) on props by #'cddr
                                               collect (substring (symbol-name k) 1)))
                  :properties ,props))
              (arr (items) `(:type "array" :items ,items)))
    (let ((str '(:type "string")))
      (json-serialize
       (obj :overallSummary str
            :groups
            (arr (obj :key str
                      :label str
                      :summary str
                      :category `(:type "string"
                                  :enum ["core" "tests" "docs" "config" "deps"
                                         "generated" "other"])
                      :critical '(:type "boolean")
                      :filePaths (arr str)
                      :fileNotes (arr (obj :path str :text str))
                      :lineNotes (arr (obj :path str
                                           :line '(:type "integer")
                                           :side '(:type "string" :enum ["new" "old"])
                                           :text str)))))))))

(defun +difftastic-groups--number-patch (patch)
  "PATCH with each hunk line's number after its marker."
  (let (old new in-hunk out)
    (dolist (line (split-string patch "\n"))
      (push (cond
             ((string-prefix-p "diff --git " line) (setq in-hunk nil) line)
             ((string-match "\\`@@ -\\([0-9]+\\)\\(?:,[0-9]+\\)? \\+\\([0-9]+\\)" line)
              (setq old (string-to-number (match-string 1 line))
                    new (string-to-number (match-string 2 line))
                    in-hunk t)
              line)
             ((not in-hunk) line)
             ((string-prefix-p "-" line)
              (prog1 (format "-%5d| %s" old (substring line 1)) (cl-incf old)))
             ((string-prefix-p "+" line)
              (prog1 (format "+%5d| %s" new (substring line 1)) (cl-incf new)))
             ((string-prefix-p " " line)
              (prog1 (format " %5d| %s" new (substring line 1))
                (cl-incf old) (cl-incf new)))
             (t line))
            out))
    (string-join (nreverse out) "\n")))

(defun +difftastic-groups--split-patch (patch)
  "PATCH as (PREAMBLE . FILE-DIFFS), split before each \"diff --git\"."
  (let ((starts (let ((pos 0) found)
                  (while (string-match "^diff --git " patch pos)
                    (push (match-beginning 0) found)
                    (setq pos (match-end 0)))
                  (nreverse found))))
    (cons (substring patch 0 (or (car starts) (length patch)))
          (cl-loop for (beg end) on starts
                   collect (substring patch beg end)))))

(defun +difftastic-groups--prompt (label kind entries patch commits)
  "The message asking claude to group a diff.
LABEL and KIND say what is diffed, ENTRIES are its stats entries, PATCH
its plain patch and COMMITS the one-line log of a range, or nil."
  (pcase-let* ((`(,preamble . ,diffs) (+difftastic-groups--split-patch patch))
               ;; Git writes numstat and patch in the same order.
               (zip (= (length diffs) (length entries)))
               (budget +difftastic-groups-patch-limit)
               (added (apply #'+ (delq nil (mapcar #'cadr entries))))
               (deleted (apply #'+ (delq nil (mapcar #'caddr entries)))))
    (concat
     (format "Change: %s %s\n\n" kind label)
     (when commits
       (format "---COMMITS--- (%d, oldest first)\n%s\n\n"
               (length (split-string commits "\n" t)) commits))
     (format "---MANIFEST--- (%d files, +%d/-%d)\n" (length entries) added deleted)
     (mapconcat (pcase-lambda (`(,path ,a ,d))
                  (concat path
                          (if a (format "  +%d/-%d" a d) "")
                          (cond ((null a) "  [binary]")
                                ((+difftastic-groups--generated-p path) "  [generated]"))))
                entries "\n")
     "\n\n---PATCH---\n"
     (+difftastic-groups--number-patch preamble)
     (mapconcat
      (lambda (pair)
        (let* ((path (car pair))
               (diff (cdr pair))
               (head (car (split-string diff "\n"))))
          (cond
           ((and path (+difftastic-groups--generated-p path))
            (concat head "\n(generated file, diff omitted)\n"))
           ((> (length diff) budget)
            (setq budget 0)
            (concat head "\n(diff omitted: patch too large)\n"))
           (t (cl-decf budget (length diff))
              (+difftastic-groups--number-patch diff)))))
      (cl-mapcar #'cons
                 (if zip (mapcar #'car entries) (make-list (length diffs) nil))
                 diffs)
      ""))))

;;; Running claude

(defvar +difftastic-groups--jobs (make-hash-table :test #'equal)
  "Running classifications: patch hash to process.")

(defvar +difftastic-groups--failures (make-hash-table :test #'equal)
  "Failed classifications: patch hash to message.")

(defun +difftastic-groups--claude-environment ()
  "`process-environment' for claude.
Without the variables in `+difftastic-groups-unset-environment'."
  ;; A bare NAME entry removes NAME from a subprocess's environment.
  (append +difftastic-groups-unset-environment process-environment))

(defun +difftastic-groups--parse-claude-output (output status timeout)
  "The structured answer in claude's json OUTPUT.
STATUS is claude's exit status and TIMEOUT the seconds after which it
was stopped, or nil.  Signal an error naming the problem otherwise."
  (when timeout
    (error "claude timed out after %ds" timeout))
  (let ((json (condition-case nil
                  (json-parse-string output :object-type 'alist :array-type 'list
                                     :false-object nil :null-object nil)
                (json-error nil))))
    (cond ((not (consp json))
           (error "claude exited %s without a result" status))
          ((alist-get 'is_error json)
           (error "claude: %s" (or (alist-get 'result json) "unknown error")))
          ((alist-get 'structured_output json))
          (t (error "claude answered without a grouping")))))

(defun +difftastic-groups--start (key prompt paths)
  "Ask claude to group the diff with patch hash KEY, from PROMPT.
PATHS are the diff's files, to check the answer against."
  (let* ((process-environment (+difftastic-groups--claude-environment))
         (out (generate-new-buffer " *difftastic-groups claude*"))
         (err (generate-new-buffer " *difftastic-groups claude stderr*"))
         (proc (make-process
                :name "difftastic-groups"
                :buffer out
                :stderr err
                :connection-type 'pipe
                :noquery t
                :command (append +difftastic-groups-claude-command
                                 (list "-p" "--output-format" "json"
                                       "--no-session-persistence"
                                       "--model" +difftastic-groups-model
                                       "--tools" ""
                                       "--system-prompt" +difftastic-groups--system-prompt
                                       "--json-schema" (+difftastic-groups--schema)))
                :sentinel
                (lambda (proc _event)
                  (unless (process-live-p proc)
                    (+difftastic-groups--finish proc key paths))))))
    (process-put proc 'stderr err)
    (process-put proc 'timer
                 (run-at-time +difftastic-groups-timeout nil
                              (lambda ()
                                (when (process-live-p proc)
                                  (process-put proc 'timed-out +difftastic-groups-timeout)
                                  (delete-process proc)))))
    (puthash key proc +difftastic-groups--jobs)
    (remhash key +difftastic-groups--failures)
    (process-send-string proc prompt)
    (process-send-eof proc)
    proc))

(defun +difftastic-groups--finish (proc key paths)
  "Keep the grouping PROC answered for patch hash KEY, or its failure."
  (let ((out (process-buffer proc))
        (err (process-get proc 'stderr)))
    (cancel-timer (process-get proc 'timer))
    (remhash key +difftastic-groups--jobs)
    (condition-case e
        (let* ((output (with-current-buffer out (buffer-string)))
               (analysis (+difftastic-groups--parse-claude-output
                          output (process-exit-status proc)
                          (process-get proc 'timed-out))))
          (+difftastic-groups--normalize analysis paths)
          (+difftastic-groups--write-cache
           key (gethash "structured_output" (json-parse-string output)))
          (kill-buffer out)
          (when (buffer-live-p err) (kill-buffer err)))
      (error
       (puthash key (error-message-string e) +difftastic-groups--failures)
       ;; Keep claude's output for a look; the next failure replaces it.
       (when-let* ((old (get-buffer "*difftastic-groups claude*")))
         (kill-buffer old))
       (with-current-buffer out
         (when (buffer-live-p err)
           (insert "\n--- stderr ---\n"
                   (with-current-buffer err (buffer-string)))
           (kill-buffer err))
         (rename-buffer "*difftastic-groups claude*"))
       (message "difftastic groups: %s (see *difftastic-groups claude*)"
                (error-message-string e))))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'difftastic-mode)
                   (equal +difftastic-groups--key key))
          (+difftastic-groups--load-view)
          (+difftastic-groups--render))))))

;;; Cache

(defun +difftastic-groups--cache-file (key extension)
  (expand-file-name (concat key extension) +difftastic-groups-cache-directory))

(defun +difftastic-groups--write-cache (key analysis)
  "Cache ANALYSIS, claude's answer as `json-parse-string' returns it, for KEY."
  (make-directory +difftastic-groups-cache-directory t)
  ;; Explicit UTF-8: Emacs would otherwise ask for a coding system, and a
  ;; question from a process sentinel hangs.
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file (+difftastic-groups--cache-file key ".json")
      (json-insert `((version . ,+difftastic-groups--version)
                     (model . ,+difftastic-groups-model)
                     (analysis . ,analysis))))))

(defun +difftastic-groups--read-cache (key)
  "Claude's cached answer for KEY, parsed, or nil."
  (let ((file (+difftastic-groups--cache-file key ".json")))
    (when (file-exists-p file)
      (let ((data (with-temp-buffer
                    (let ((coding-system-for-read 'utf-8))
                      (insert-file-contents file))
                    (json-parse-buffer :object-type 'alist :array-type 'list
                                       :false-object nil :null-object nil))))
        (when (eql (alist-get 'version data) +difftastic-groups--version)
          (alist-get 'analysis data))))))

(defun +difftastic-groups--read-marks (key)
  "Keys of the groups marked reviewed for patch hash KEY."
  (let ((file (+difftastic-groups--cache-file key ".marks")))
    (when (file-exists-p file)
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8))
          (insert-file-contents file))
        (read (current-buffer))))))

(defun +difftastic-groups--write-marks (key marks)
  (make-directory +difftastic-groups-cache-directory t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file (+difftastic-groups--cache-file key ".marks")
      (prin1 marks (current-buffer)))))

;;; The diff behind a buffer

(defun +difftastic-groups--git (&rest args)
  "Output of git ARGS, or nil when git fails."
  (with-temp-buffer
    (when (eql 0 (apply #'process-file "git" nil '(t nil) nil args))
      (buffer-string))))

(defun +difftastic-groups--patch-command (git-command)
  "GIT-COMMAND changed to print a plain patch, and a commit's message."
  (let ((show (member "show" git-command)))
    ;; Not `mapcan': it would splice later arguments onto the quoted
    ;; constant at the end of the backquote, and every call would add to it.
    (apply #'append
           (mapcar (lambda (arg)
                     (if (equal arg "--ext-diff")
                         `("--no-ext-diff" "--no-color" ,@(and show '("--format=%B")))
                       (list arg)))
                   (cdr git-command)))))

(defun +difftastic-groups--range ()
  "The range this buffer diffs, as A..B, or nil."
  (let ((rev (alist-get 'rev-or-range difftastic--metadata)))
    (and (stringp rev) (string-match-p "\\.\\." rev)
         (not (member "show" (alist-get 'git-command difftastic--metadata)))
         (replace-regexp-in-string "\\.\\.\\." ".." rev))))

(defun +difftastic-groups--kind ()
  "What this buffer diffs, in words."
  (let ((rev (alist-get 'rev-or-range difftastic--metadata)))
    (cond ((member "show" (alist-get 'git-command difftastic--metadata)) "commit")
          ((+difftastic-groups--range) "range")
          ((stringp rev) "working tree against")
          (t "changes:"))))

(defun +difftastic-groups--auto-p (paths)
  "Whether to classify PATHS by itself: a large commit or range."
  (and +difftastic-groups-auto-min-files
       (>= (length paths) +difftastic-groups-auto-min-files)
       (member (+difftastic-groups--kind) '("commit" "range"))))

(defun +difftastic-groups--paths ()
  (mapcar #'car +difftastic-stats--entries))

(defun +difftastic-groups--load-view ()
  "Show claude's cached grouping, or else the path rules'."
  (setq +difftastic-groups--view
        (or (when-let* ((key +difftastic-groups--key)
                        (analysis (condition-case e
                                      (+difftastic-groups--read-cache key)
                                    (error (puthash key (format "unreadable cache: %s"
                                                                (error-message-string e))
                                                    +difftastic-groups--failures)
                                           nil))))
              (condition-case nil
                  (+difftastic-groups--normalize analysis (+difftastic-groups--paths))
                (+difftastic-groups-invalid
                 (puthash key "cached grouping is malformed" +difftastic-groups--failures)
                 nil)))
            (+difftastic-groups--rule-view +difftastic-stats--entries))
        +difftastic-groups--marks
        (and +difftastic-groups--key
             (+difftastic-groups--read-marks +difftastic-groups--key))))

(defun +difftastic-groups-h ()
  "Group the files of this difftastic buffer, and classify them when due."
  (setq +difftastic-groups--view nil
        +difftastic-groups--steps nil)
  (when-let* ((command (alist-get 'git-command difftastic--metadata))
              ((cdr +difftastic-stats--entries)))
    (let ((default-directory (alist-get 'default-directory difftastic--metadata)))
      (setq +difftastic-groups--patch
            (apply #'+difftastic-groups--git
                   (+difftastic-groups--patch-command command))
            +difftastic-groups--key
            (and +difftastic-groups--patch (sha1 +difftastic-groups--patch))
            +difftastic-groups--file-statuses
            (and +difftastic-groups--patch
                 (+difftastic-groups--statuses +difftastic-groups--patch
                                               +difftastic-stats--entries))))
    (+difftastic-groups--load-view)
    (+difftastic-groups--render)
    (let ((key +difftastic-groups--key))
      (when (and key
                 (eq (plist-get +difftastic-groups--view :source) 'rules)
                 (not (gethash key +difftastic-groups--jobs))
                 (not (gethash key +difftastic-groups--failures))
                 (+difftastic-groups--auto-p (+difftastic-groups--paths)))
        (+difftastic-groups-classify)))))

;;; Rendering

(defun +difftastic-groups--segments ()
  "Each file's output as (PATH BEG END HEADERS), in buffer order.
A file's output begins at a header line, \"PATH --- …\", after a blank
line; HEADERS are the positions of its header lines."
  (let ((known (make-hash-table :test #'equal))
        segments)
    (dolist (path (+difftastic-groups--paths)) (puthash path t known))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (or (bobp) (save-excursion (forward-line -1) (looking-at-p "$")))
                   (looking-at "\\(.+?\\) --- ")
                   (gethash (match-string-no-properties 1) known))
          (let ((path (match-string-no-properties 1)))
            (if (equal path (caar segments))
                (push (point) (nth 3 (car segments)))
              (push (list path (point) nil (list (point))) segments))))
        (forward-line 1)))
    (let ((end (point-max)))
      (dolist (seg segments)
        (setf (nth 2 seg) end
              (nth 3 seg) (nreverse (nth 3 seg))
              end (nth 1 seg))))
    (nreverse segments)))

(defun +difftastic-groups--order (view)
  "Paths in the order VIEW shows them."
  (mapcan (lambda (g) (copy-sequence (plist-get g :files)))
          (plist-get view :groups)))

(defun +difftastic-groups--reorder (segments order)
  "Rearrange the file outputs in SEGMENTS into ORDER.
Return non-nil when the buffer changed."
  (let ((current (mapcar #'car segments))
        (wanted (seq-filter (lambda (p) (assoc p segments)) order)))
    (unless (equal current wanted)
      (let* ((beg (nth 1 (car segments)))
             (texts (mapcar (lambda (seg)
                              (cons (car seg)
                                    (concat (string-trim-right
                                             (buffer-substring (nth 1 seg) (nth 2 seg))
                                             "\n+")
                                            "\n\n")))
                            segments)))
        (delete-region beg (point-max))
        (goto-char beg)
        (dolist (path wanted)
          (insert (alist-get path texts nil nil #'equal)))
        t))))

(defun +difftastic-groups--width ()
  (let ((win (get-buffer-window)))
    (max 40 (min 100 (- (if win (window-width win) 80) 4)))))

(defun +difftastic-groups--fill (text indent width)
  "TEXT filled to WIDTH, every line starting with INDENT."
  (with-temp-buffer
    (insert indent (string-trim text))
    (let ((fill-column width)
          (fill-prefix indent)
          (adaptive-fill-mode nil))
      (fill-region (point-min) (point-max)))
    (buffer-string)))

(defun +difftastic-groups--counts (paths)
  "\"N files  +A −D\" for PATHS."
  (let ((entries (seq-filter (lambda (e) (member (car e) paths))
                             +difftastic-stats--entries)))
    (concat (format "%d file%s  " (length paths) (if (cdr paths) "s" ""))
            (+difftastic-stats--counts
             (apply #'+ (delq nil (mapcar #'cadr entries)))
             (apply #'+ (delq nil (mapcar #'caddr entries)))))))

(defun +difftastic-groups--source-tag ()
  "Where the grouping came from, and what claude is up to."
  (let ((key +difftastic-groups--key))
    (cond ((eq (plist-get +difftastic-groups--view :source) 'llm)
           (propertize "[claude]" 'face 'success))
          ((and key (gethash key +difftastic-groups--jobs))
           (propertize "[rules · claude classifying…]" 'face 'shadow))
          ((and key (gethash key +difftastic-groups--failures))
           (propertize (format "[rules · claude failed: %s]"
                               (gethash key +difftastic-groups--failures))
                       'face 'warning))
          (t (propertize "[rules · C classifies]" 'face 'shadow)))))

(defun +difftastic-groups--plain (text)
  "TEXT that difftastic can't take for a chunk header."
  (replace-regexp-in-string " --- " " — " (or text "")))

(defun +difftastic-groups--overview ()
  "The overview text: summary, groups, and warnings."
  (let* ((view +difftastic-groups--view)
         (groups (plist-get view :groups))
         (width (+difftastic-groups--width))
         (label-width (min 32 (apply #'max 8 (mapcar (lambda (g)
                                                        (length (plist-get g :label)))
                                                      groups)))))
    (concat
     (propertize "Review guide" 'font-lock-face '+difftastic-groups-heading)
     (propertize (format "  %d groups" (length groups)) 'font-lock-face 'shadow)
     "\n"
     (when-let* ((summary (plist-get view :summary)))
       (concat (propertize (+difftastic-groups--fill
                            (+difftastic-groups--plain summary) "" width)
                           'font-lock-face '+difftastic-groups-summary)
               "\n"))
     "\n"
     (mapconcat
      (lambda (group)
        (let ((i (seq-position groups group)))
          (propertize
           (concat
            (format " %2d " (1+ i))
            (if (member (plist-get group :key) +difftastic-groups--marks)
                (propertize "✓" 'font-lock-face 'success)
              " ")
            " "
            (propertize (truncate-string-to-width
                         (+difftastic-groups--plain (plist-get group :label))
                         label-width nil ?\s)
                        'font-lock-face 'bold)
            (if (plist-get group :critical)
                (propertize " ⚠" 'font-lock-face 'error)
              "  ")
            (propertize (format "  %-9s " (plist-get group :category))
                        'font-lock-face 'shadow)
            (+difftastic-groups--counts (plist-get group :files))
            "\n")
           '+difftastic-groups-step i)))
      groups "")
     (mapconcat (lambda (w) (propertize (concat " ⚠ " (+difftastic-groups--plain w) "\n")
                                        'font-lock-face 'warning))
                (plist-get view :warnings) "")
     "\n")))

(defun +difftastic-groups--heading (group i n)
  "The heading shown above GROUP, the Ith of N."
  (let ((width (+difftastic-groups--width)))
    (concat
     (propertize (concat "━━ " (format "%d/%d  " (1+ i) n)
                         (plist-get group :label)
                         (if (plist-get group :critical) "  ⚠ critical" "")
                         (if (member (plist-get group :key) +difftastic-groups--marks)
                             "  ✓ reviewed" "")
                         "  "
                         (propertize (format "%s · " (plist-get group :category))
                                     'face 'shadow)
                         (+difftastic-groups--counts (plist-get group :files))
                         "\n")
                 'face '+difftastic-groups-heading)
     (when-let* ((summary (plist-get group :summary)))
       (concat (propertize (+difftastic-groups--fill summary "   " width)
                           'face '+difftastic-groups-summary)
               "\n"))
     "\n")))

(defun +difftastic-groups--note (text &optional label)
  "TEXT as a note line, after LABEL."
  (let ((mark "  🤖 "))
    ;; `default' last: without a background of its own, an overlay string
    ;; takes the tint of the changed line it sits on.
    (propertize (concat mark
                        (string-trim-left
                         (+difftastic-groups--fill (concat label text)
                                                   (make-string (string-width mark) ?\s)
                                                   (+difftastic-groups--width))))
                'face '(+difftastic-groups-note default))))

(defun +difftastic-groups--overlay (beg end prop &rest props)
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov '+difftastic-groups prop)
    (overlay-put ov 'evaporate t)
    (while props
      (overlay-put ov (pop props) (pop props)))
    ov))

(defun +difftastic-groups--left-number (bol right prev)
  "The old line number of the side-by-side row at BOL, or nil.
RIGHT is the row's parsed right number; a continuation row (dots) has
PREV's number.  difftastic's parser takes a padded number for blanks."
  (save-excursion
    (goto-char bol)
    (when (and (looking-at " *\\([0-9]+\\|\\.+\\) ")
               (or (null right) (< (match-beginning 1) (nth 1 right))))
      (if (eq (char-after (match-beginning 1)) ?.)
          prev
        (string-to-number (match-string 1))))))

(defun +difftastic-groups--field (bol eol from to prev)
  "The number in columns FROM to TO of the row at BOL, ending at EOL.
Dots, difft's mark for a wrapped line, stand for PREV."
  (let ((text (string-trim (buffer-substring-no-properties
                            (min (+ bol from) eol) (min (+ bol to) eol)))))
    (cond ((string-match-p "\\`[0-9]+\\'" text) (string-to-number text))
          ((string-match-p "\\`\\.+\\'" text) prev))))

(defun +difftastic-groups--rows (bounds layout)
  "Rows of the chunk at BOUNDS as (EOL OLD NEW), with their line numbers.
LAYOUT is how difft drew the chunk: `added' or `deleted' (one column of
numbers), `inline' (an old and a new column) or `side-by-side'."
  (if (eq layout 'side-by-side)
      (let (prev)
        (mapcar (pcase-lambda (`((,bol ,eol) ,_ ,right))
                  (let ((old (+difftastic-groups--left-number bol right prev)))
                    (setq prev (or old prev))
                    (list eol old (car right))))
                (difftastic--parse-side-by-side-chunk bounds)))
    (save-excursion
      (goto-char (car bounds))
      (forward-line 1)
      ;; Where the number columns end, from the first row showing them all.
      (let ((ends (save-excursion
                    (when (re-search-forward (if (eq layout 'inline)
                                                 "^ *\\([0-9]+\\) +\\([0-9]+\\) "
                                               "^ *\\([0-9]+\\) ")
                                             (cdr bounds) t)
                      (list (- (match-end 1) (pos-bol))
                            (and (match-end 2) (- (match-end 2) (pos-bol)))))))
            rows prev-old prev-new)
        (while (and ends (< (point) (cdr bounds)))
          (let* ((bol (point))
                 (eol (pos-eol))
                 (first (+difftastic-groups--field bol eol 0 (car ends)
                                                   (if (eq layout 'added) prev-new prev-old)))
                 (old (pcase layout ('added nil) (_ first)))
                 (new (pcase layout
                        ('added first)
                        ('deleted nil)
                        (_ (+difftastic-groups--field bol eol (1+ (car ends)) (cadr ends)
                                                      prev-new)))))
            (setq prev-old (or old prev-old)
                  prev-new (or new prev-new))
            (push (list eol old new) rows))
          (forward-line 1))
        (nreverse rows)))))

(defun +difftastic-groups--statuses (patch entries)
  "Alist of PATH to `added' or `deleted' for the files PATCH adds or deletes.
ENTRIES are the stats entries, in the patch's file order."
  (let ((diffs (cdr (+difftastic-groups--split-patch patch))))
    (when (= (length diffs) (length entries))
      (cl-loop for diff in diffs
               for (path) in entries
               for head = (substring diff 0 (string-match "^@@" diff))
               for status = (cond ((string-match-p "^new file mode" head) 'added)
                                  ((string-match-p "^deleted file mode" head) 'deleted))
               when status collect (cons path status)))))

(defun +difftastic-groups--line-end (segment line side)
  "End of the last row in SEGMENT that shows LINE on SIDE, or nil.
SIDE is \"new\" or \"old\"."
  (let ((status (alist-get (car segment) +difftastic-groups--file-statuses nil nil #'equal))
        found)
    (save-excursion
      (dolist (header (nth 3 segment))
        (goto-char header)
        (when-let* (((looking-at (difftastic--chunk-regexp t)))
                    (bounds (difftastic--chunk-bounds))
                    (bounds (cons (car bounds) (min (cdr bounds) (nth 2 segment)))))
          (pcase-dolist (`(,eol ,old ,new)
                         (+difftastic-groups--rows
                          bounds
                          (or status
                              (if (eq (difftastic--classify-chunk bounds) 'single-column)
                                  'inline
                                'side-by-side))))
            (when (eql (if (equal side "old") old new) line)
              (setq found eol))))))
    found))

(defun +difftastic-groups--annotate (segments)
  "Add headings and notes to the file outputs in SEGMENTS."
  (let* ((groups (plist-get +difftastic-groups--view :groups))
         (n (length groups))
         (unplaced (make-hash-table :test #'equal)))
    (setq +difftastic-groups--steps nil)
    (cl-loop
     for group in groups for i from 0
     for seg = (seq-find (lambda (s) (member (car s) (plist-get group :files))) segments)
     when seg do
     (+difftastic-groups--overlay (nth 1 seg) (save-excursion (goto-char (nth 1 seg))
                                                               (pos-eol))
                                  'heading
                                  'before-string (+difftastic-groups--heading group i n))
     (push (cons group (copy-marker (nth 1 seg))) +difftastic-groups--steps))
    (setq +difftastic-groups--steps (nreverse +difftastic-groups--steps))
    ;; Each note overlay keeps its notes as (REF . TEXT) under `notes',
    ;; for `+difftastic-groups-copy-notes'.  File notes go under the file
    ;; header, followed by line notes difft shows no row for.
    (dolist (group groups)
      (pcase-dolist (`(,path . ,text) (plist-get group :file-notes))
        (push (list (cons path text) text nil) (gethash path unplaced))))
    (dolist (group groups)
      (dolist (note (plist-get group :line-notes))
        (let* ((path (plist-get note :path))
               (line (plist-get note :line))
               (ref (cons (format "%s:%d" path line) (plist-get note :text)))
               (seg (assoc path segments))
               (eol (and seg (+difftastic-groups--line-end
                              seg line (plist-get note :side)))))
          (if eol
              (+difftastic-groups--overlay
               (save-excursion (goto-char eol) (pos-bol)) eol 'note
               'notes (list ref)
               'after-string (concat "\n" (+difftastic-groups--note (cdr ref))))
            (push (list ref (cdr ref) (format "line %d: " line))
                  (gethash path unplaced))))))
    (maphash
     (lambda (path notes)
       (when-let* ((seg (assoc path segments)))
         (save-excursion
           (goto-char (nth 1 seg))
           (forward-line 1)
           (let* ((notes (reverse notes))
                  (refs (mapcar #'car notes))
                  (text (mapconcat (pcase-lambda (`(,_ ,text ,label))
                                     (concat (+difftastic-groups--note text label) "\n"))
                                   notes "")))
             (if (< (point) (point-max))
                 (+difftastic-groups--overlay (point) (pos-eol) 'file-note
                                              'notes refs 'before-string text)
               (+difftastic-groups--overlay (pos-bol 0) (pos-eol 0) 'file-note
                                            'notes refs
                                            'after-string (concat "\n" text)))))))
     unplaced)))

;;; Copying

(defun +difftastic-groups--filter-substring (beg end &optional delete)
  "The text between BEG and END as shown: with headings and notes.
For `filter-buffer-substring-function', so copying a region takes the
overlay text along.  Plain text: difft's colors mean nothing elsewhere."
  (if delete
      (buffer-substring--filter beg end delete)
    (let ((beg (min beg end))
          (end (max beg end))
          inserts)
      (dolist (ov (overlays-in beg end))
        (when (overlay-get ov '+difftastic-groups)
          (when-let* ((text (overlay-get ov 'before-string))
                      ((<= beg (overlay-start ov)))
                      ((< (overlay-start ov) end)))
            (push (list (overlay-start ov) 0 text) inserts))
          (when-let* ((text (overlay-get ov 'after-string))
                      ((< beg (overlay-end ov)))
                      ((<= (overlay-end ov) end)))
            (push (list (overlay-end ov) 1 text) inserts))))
      (setq inserts (sort inserts (lambda (a b) (or (< (car a) (car b))
                                                     (and (= (car a) (car b))
                                                          (> (cadr a) (cadr b)))))))
      (let ((pos beg) parts)
        (pcase-dolist (`(,at ,_ ,text) inserts)
          (push (buffer-substring-no-properties pos at) parts)
          (push (substring-no-properties text) parts)
          (setq pos at))
        (push (buffer-substring-no-properties pos end) parts)
        (apply #'concat (nreverse parts))))))

(defun +difftastic-groups--notes-in (beg end)
  "The (REF . TEXT) notes of overlays between BEG and END, in order."
  (mapcan (lambda (ov) (copy-sequence (overlay-get ov 'notes)))
          (sort (seq-filter (lambda (ov) (overlay-get ov 'notes))
                            (overlays-in beg end))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun +difftastic-groups-copy-notes ()
  "Copy claude's notes as \"PATH:LINE: TEXT\" lines.
The notes on the line at point, or else all notes on the file at point."
  (interactive nil difftastic-mode)
  (let ((notes (or (+difftastic-groups--notes-in (pos-bol) (pos-eol))
                   (when-let* ((seg (seq-find (lambda (s) (and (<= (nth 1 s) (point))
                                                               (< (point) (nth 2 s))))
                                              (+difftastic-groups--segments))))
                     (+difftastic-groups--notes-in (nth 1 seg) (nth 2 seg)))
                   (user-error "No notes here"))))
    (kill-new (mapconcat (pcase-lambda (`(,ref . ,text)) (format "%s: %s\n" ref text))
                         notes ""))
    (message "Copied %d note%s" (length notes) (if (cdr notes) "s" ""))))

(defun +difftastic-groups--anchor (pos segments)
  "POS as (PATH . OFFSET) in SEGMENTS, or (nil . POS) before them."
  (or (seq-some (lambda (seg)
                  (and (<= (nth 1 seg) pos) (< pos (nth 2 seg))
                       (cons (car seg) (- pos (nth 1 seg)))))
                segments)
      (cons nil pos)))

(defun +difftastic-groups--resolve (anchor segments)
  (if-let* ((seg (assoc (car anchor) segments)))
      (min (+ (nth 1 seg) (cdr anchor)) (nth 2 seg))
    (min (cdr anchor) (point-max))))

(defun +difftastic-groups--clear ()
  "Remove the overview and the overlays a render added."
  (remove-overlays (point-min) (point-max) '+difftastic-groups 'heading)
  (remove-overlays (point-min) (point-max) '+difftastic-groups 'note)
  (remove-overlays (point-min) (point-max) '+difftastic-groups 'file-note)
  (when-let* ((beg (text-property-any (point-min) (point-max)
                                      '+difftastic-groups-overview t)))
    (delete-region beg (or (text-property-not-all beg (point-max)
                                                  '+difftastic-groups-overview t)
                           (point-max)))))

(defun +difftastic-groups--refresh-indicators ()
  "Redo difftastic's fringe indicators after file outputs moved."
  (when (consp difftastic-visibility-indicator)
    (remove-overlays (point-min) (point-max) 'difftastic-visibility-indicator t)
    (save-excursion
      (goto-char (point-max))
      (difftastic--add-visibility-indicators (point-min))
      (goto-char (point-min))
      (while (re-search-forward (difftastic--chunk-regexp nil) nil t)
        (when (memq :hidden (get-text-property (pos-bol) 'difftastic))
          (difftastic--update-visibility-indcators
           (car difftastic-visibility-indicator) (pos-bol) (pos-eol)))))))

(defun +difftastic-groups--render ()
  "Show `+difftastic-groups--view' in this buffer."
  (let ((inhibit-read-only t)
        (segments (+difftastic-groups--segments)))
    (when (and +difftastic-groups--view segments)
      (let* ((windows (get-buffer-window-list nil nil t))
             (anchors (cons (+difftastic-groups--anchor (point) segments)
                            (mapcar (lambda (w)
                                      (list w (+difftastic-groups--anchor (window-point w) segments)
                                            (+difftastic-groups--anchor (window-start w) segments)))
                                    windows))))
        (+difftastic-groups--clear)
        (setq segments (+difftastic-groups--segments))
        (when (+difftastic-groups--reorder segments
                                           (+difftastic-groups--order +difftastic-groups--view))
          (+difftastic-groups--refresh-indicators))
        (goto-char (nth 1 (car (+difftastic-groups--segments))))
        (insert (propertize (+difftastic-groups--overview)
                            '+difftastic-groups-overview t))
        (setq segments (+difftastic-groups--segments))
        (+difftastic-groups--annotate segments)
        (setq-local filter-buffer-substring-function
                    #'+difftastic-groups--filter-substring)
        (+difftastic-stats--annotate +difftastic-stats--entries
                                     (+difftastic-stats--label
                                      (alist-get 'rev-or-range difftastic--metadata)))
        (setq header-line-format
              (list header-line-format '(:eval (+difftastic-groups--header-status))))
        (goto-char (+difftastic-groups--resolve (car anchors) segments))
        (pcase-dolist (`(,w ,point ,start) (cdr anchors))
          (set-window-start w (+difftastic-groups--resolve start segments) t)
          (set-window-point w (+difftastic-groups--resolve point segments)))
        (set-buffer-modified-p nil)))))

;;; Stepping through the groups

(defun +difftastic-groups--step-index (&optional pos)
  "Index of the group at POS in `+difftastic-groups--steps', or nil."
  (let ((pos (or pos (point))))
    (cl-loop for (_ . marker) in +difftastic-groups--steps for i from 0
             when (<= marker pos) maximize i)))

(defun +difftastic-groups--header-status ()
  "The group at point and where the grouping came from, for the header line."
  (when-let* ((view +difftastic-groups--view))
    (let* ((i (+difftastic-groups--step-index))
           (steps +difftastic-groups--steps)
           (group (car (nth (or i 0) steps))))
      (concat "  "
              (if i
                  (format "▸ %d/%d %s"
                          (1+ (seq-position (plist-get view :groups) group))
                          (length (plist-get view :groups))
                          (plist-get group :label))
                (format "%d groups" (length (plist-get view :groups))))
              "  " (+difftastic-groups--source-tag)))))

(defun +difftastic-groups--goto (i)
  "Show the Ith step's group at the top of the window."
  (goto-char (cdr (nth i +difftastic-groups--steps)))
  (when (eq (window-buffer) (current-buffer))
    (recenter 0)))

(defun +difftastic-groups--require-steps ()
  (unless +difftastic-groups--steps
    (user-error "No groups in this buffer")))

(defun +difftastic-groups-next ()
  "Go to the next group."
  (interactive nil difftastic-mode)
  (+difftastic-groups--require-steps)
  (let ((i (+difftastic-groups--step-index)))
    (if (and i (>= (1+ i) (length +difftastic-groups--steps)))
        (user-error "No more groups")
      (+difftastic-groups--goto (if i (1+ i) 0)))))

(defun +difftastic-groups-previous ()
  "Go to the previous group, or the start of this one."
  (interactive nil difftastic-mode)
  (+difftastic-groups--require-steps)
  (let ((i (+difftastic-groups--step-index)))
    (cond ((null i) (user-error "No previous group"))
          ((> (point) (cdr (nth i +difftastic-groups--steps)))
           (+difftastic-groups--goto i))
          ((zerop i) (goto-char (point-min)))
          (t (+difftastic-groups--goto (1- i))))))

(defun +difftastic-groups-jump ()
  "Go to the group of the overview line at point."
  (interactive nil difftastic-mode)
  (let* ((i (get-text-property (point) '+difftastic-groups-step))
         (group (and i (nth i (plist-get +difftastic-groups--view :groups))))
         (step (and group (seq-position +difftastic-groups--steps group
                                        (lambda (s g) (eq (car s) g))))))
    (cond (step (+difftastic-groups--goto step))
          (group (user-error "difft shows no output for this group"))
          (t (user-error "Not on a group")))))

(defun +difftastic-groups-toggle-reviewed ()
  "Mark the group at point reviewed and go to the next unreviewed one.
On a reviewed group, unmark it."
  (interactive nil difftastic-mode)
  (+difftastic-groups--require-steps)
  (let* ((i (or (+difftastic-groups--step-index)
                (user-error "Not in a group")))
         (key (plist-get (car (nth i +difftastic-groups--steps)) :key))
         (marking (not (member key +difftastic-groups--marks))))
    (setq +difftastic-groups--marks
          (if marking
              (cons key +difftastic-groups--marks)
            (delete key +difftastic-groups--marks)))
    (when +difftastic-groups--key
      (+difftastic-groups--write-marks +difftastic-groups--key +difftastic-groups--marks))
    (+difftastic-groups--render)
    (when marking
      (if-let* ((next (cl-position-if-not
                       (lambda (s) (member (plist-get (car s) :key) +difftastic-groups--marks))
                       +difftastic-groups--steps)))
          (+difftastic-groups--goto next)
        (message "All groups reviewed")))))

(defun +difftastic-groups-classify (&optional force)
  "Group this diff with claude, in the background.
With FORCE (prefix argument), classify again even when a grouping is
cached or the last attempt failed."
  (interactive "P" difftastic-mode)
  (let ((key (or +difftastic-groups--key
                 (user-error "Nothing to classify in this buffer"))))
    (cond
     ((gethash key +difftastic-groups--jobs)
      (message "Already classifying"))
     ((and (not force) (eq (plist-get +difftastic-groups--view :source) 'llm))
      (message "Already classified; %s reclassifies"
               (substitute-command-keys
                "\\[universal-argument] \\[+difftastic-groups-classify]")))
     ((file-remote-p (alist-get 'default-directory difftastic--metadata))
      (user-error "Classifying remote diffs is not supported"))
     (t
      (let ((default-directory (alist-get 'default-directory difftastic--metadata)))
        (+difftastic-groups--start
         key
         (+difftastic-groups--prompt
          (+difftastic-stats--label (alist-get 'rev-or-range difftastic--metadata))
          (+difftastic-groups--kind)
          +difftastic-stats--entries
          +difftastic-groups--patch
          (when-let* ((range (+difftastic-groups--range)))
            (string-trim (or (+difftastic-groups--git
                              "log" "--reverse" "--no-color" "--format=%h %s" range)
                             ""))))
         (+difftastic-groups--paths)))))
    (force-mode-line-update)))

;; Run after difftastic-stats' advice (depth 0), which collects the
;; entries this needs.
(define-advice difftastic--run-command-sentinel
    (:after (process &rest _) +difftastic-groups -50)
  (when (and (eq (process-status process) 'exit)
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (+difftastic-groups-h))))

;;; difftastic-groups.el ends here
