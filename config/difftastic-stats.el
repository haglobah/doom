;;; difftastic-stats.el --- Line counts in difftastic buffers -*- lexical-binding: t; -*-

;; difft prints no diffstat.  After a git-backed difftastic buffer fills,
;; rerun its git command with --numstat and show the counts: after each
;; file header ("m.py --- Python  +2 −1") and, for the whole commit or
;; range, in the header line (" HEAD~3..HEAD  4 files  +40 −12").

(require 'difftastic)
(require 'magit-diff)                   ; magit-diffstat-added/-removed faces

(defun +difftastic-stats--command (git-command)
  "The --numstat variant of GIT-COMMAND, a `git diff' or `git show' call."
  (let ((show (member "show" git-command)))
    (apply #'append
           (mapcar (lambda (arg)
                     (if (equal arg "--ext-diff")
                         `("--no-ext-diff" "--numstat" "-z" ,@(and show '("--format=")))
                       (list arg)))
                   git-command))))

(defun +difftastic-stats--parse (output)
  "Parse `git --numstat -z' OUTPUT into (PATH ADDED DELETED) entries.
ADDED and DELETED are nil for binary files.  A rename uses its new path."
  (let ((fields (split-string output "\0"))
        stats)
    (while fields
      (let ((field (pop fields)))
        (when (string-match "\\`\\([-0-9]+\\)\t\\([-0-9]+\\)\t\\(.*\\)\\'" field)
          (let ((added (match-string 1 field))
                (deleted (match-string 2 field))
                (path (match-string 3 field)))
            (when (string-empty-p path)   ; rename: old\0new\0 follow
              (pop fields)
              (setq path (pop fields)))
            (push (list path
                        (and (not (equal added "-")) (string-to-number added))
                        (and (not (equal deleted "-")) (string-to-number deleted)))
                  stats)))))
    (nreverse stats)))

(defun +difftastic-stats--counts (added deleted)
  "ADDED and DELETED as \"+A −D\", or \"binary\" when they are nil."
  (if (and added deleted)
      (concat (propertize (format "+%d" added) 'face 'magit-diffstat-added)
              " "
              (propertize (format "−%d" deleted) 'face 'magit-diffstat-removed))
    (propertize "binary" 'face 'shadow)))

(defun +difftastic-stats--label (rev-or-range)
  "What the buffer diffs, for the header line."
  (pcase rev-or-range
    ((pred stringp) rev-or-range)
    ('nil "unstaged")
    (_ (symbol-name rev-or-range))))

(defun +difftastic-stats--annotate (stats label)
  "Show STATS after the file headers and, with LABEL, in the header line."
  (remove-overlays (point-min) (point-max) '+difftastic-stats t)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (difftastic--chunk-regexp t) nil t)
      (when-let* ((entry (assoc (string-trim (match-string-no-properties 1))
                                stats))
                  (ov (make-overlay (pos-eol) (pos-eol))))
        (overlay-put ov '+difftastic-stats t)
        (overlay-put ov 'after-string
                     (concat "  " (apply #'+difftastic-stats--counts (cdr entry)))))))
  (let ((files (length stats)))
    (setq header-line-format
          (concat " " (propertize label 'face 'bold)
                  (format "  %d file%s  " files (if (= files 1) "" "s"))
                  (+difftastic-stats--counts
                   (apply #'+ (delq nil (mapcar #'cadr stats)))
                   (apply #'+ (delq nil (mapcar #'caddr stats))))))))

(defun +difftastic-stats-h ()
  "Count changed lines for the git command behind this difftastic buffer."
  (when-let* ((command (alist-get 'git-command difftastic--metadata)))
    (let* ((default-directory (alist-get 'default-directory difftastic--metadata))
           (label (+difftastic-stats--label
                   (alist-get 'rev-or-range difftastic--metadata)))
           (numstat (+difftastic-stats--command command))
           status output)
      (with-temp-buffer
        (setq status (apply #'process-file (car numstat) nil '(t nil) nil
                            (cdr numstat))
              output (buffer-string)))
      (if (eql status 0)
          (+difftastic-stats--annotate (+difftastic-stats--parse output) label)
        (remove-overlays (point-min) (point-max) '+difftastic-stats t)
        (setq header-line-format
              (propertize (format " %s  line counts unavailable (git exit %s)"
                                  label status)
                          'face 'warning))))))

;; Both the first run and `difftastic-rerun' (g) set the buffer's metadata
;; in the action this sentinel calls, so count after it.
(define-advice difftastic--run-command-sentinel
    (:after (process &rest _) +difftastic-stats)
  (when (and (eq (process-status process) 'exit)
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (+difftastic-stats-h))))

;;; difftastic-stats.el ends here
