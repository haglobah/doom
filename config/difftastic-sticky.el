;;; difftastic-sticky.el --- Sticky part and file bars in difftastic -*- lexical-binding: t; -*-

;; Under the header line (the commit), a difftastic window shows a bar
;; with the file that has scrolled off its top, and above it, once the
;; buffer has groups, the part (group) with its summary:
;;
;;   HEAD  12 files  +40 −12        ← header line
;;   2/5 Parser — Adds the parser.  ← part
;;   lib/parse.el  +30 −2           ← file
;;
;; The bars name what lies above the window's first line.  When the next
;; file's own header reaches the top, the bar still names the old file;
;; once that header scrolls off, the bar takes its name: the header
;; slides under the bar.
;;
;; A header line holds one line only, so the bars are a `before-string'
;; at `window-start', one overlay per window, moved as the window scrolls.

(require 'difftastic)

(defvar +difftastic-stats--entries)
(defvar +difftastic-groups--steps)
(defvar +difftastic-groups--view)
(declare-function +difftastic-stats--counts "difftastic-stats")

(defun +difftastic-sticky--file (pos)
  "Path of the file whose output holds POS, or nil."
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (let ((regexp (difftastic--chunk-regexp t))
          path)
      (while (and (not path) (re-search-backward regexp nil t))
        (unless (difftastic--point-at-added-removed-p)
          (setq path (string-trim (match-string-no-properties 1)))))
      path)))

(defun +difftastic-sticky--group (pos)
  "The group whose output holds POS, or nil."
  (let (group)
    (pcase-dolist (`(,g . ,marker) (bound-and-true-p +difftastic-groups--steps))
      (when (<= marker pos)
        (setq group g)))
    group))

(defun +difftastic-sticky--line (text face width)
  "TEXT as one bar line in FACE, cut to WIDTH."
  (let ((line (concat (truncate-string-to-width (concat " " text) width) "\n")))
    ;; Behind TEXT's own faces; `default' last: without a background of
    ;; its own, an overlay string takes the tint of the line it sits on.
    (add-face-text-property 0 (length line) face t line)
    (add-face-text-property 0 (length line) 'default t line)
    line))

(defun +difftastic-sticky--part (group width)
  (+difftastic-sticky--line
   (if group
       (let* ((groups (plist-get +difftastic-groups--view :groups))
              (summary (plist-get group :summary)))
         (concat (format "%d/%d " (1+ (or (seq-position groups group) 0))
                         (length groups))
                 (plist-get group :label)
                 (when summary
                   (propertize (concat " — " (car (split-string summary "\n")))
                               'face 'shadow))))
     "")
   '+difftastic-groups-heading width))

(defun +difftastic-sticky--file-line (path width)
  (+difftastic-sticky--line
   (if path
       (concat (propertize path 'face 'bold)
               (when-let* ((entry (assoc path (bound-and-true-p +difftastic-stats--entries))))
                 (concat "  " (apply #'+difftastic-stats--counts (cdr entry)))))
     "")
   '+difftastic-hunk-header width))

(defun +difftastic-sticky--text (pos width)
  "The bars for a window whose top hides POS."
  (concat (when (bound-and-true-p +difftastic-groups--steps)
            (+difftastic-sticky--part (+difftastic-sticky--group pos) width))
          (+difftastic-sticky--file-line (+difftastic-sticky--file pos) width)))

(defun +difftastic-sticky--overlay (window)
  (seq-find (lambda (ov) (eq (overlay-get ov '+difftastic-sticky) window))
            (overlays-in (point-min) (point-max))))

(defun +difftastic-sticky-update (&optional window start)
  "Move WINDOW's bars to START, its first line, and name what is above it.
Drops the bars of windows that no longer show this buffer."
  (let ((window (or window (selected-window))))
    (with-current-buffer (window-buffer window)
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when-let* ((w (overlay-get ov '+difftastic-sticky))
                    ((not (and (window-live-p w) (eq (window-buffer w) (current-buffer))))))
          (delete-overlay ov)))
      (let ((start (or start (window-start window)))
            (ov (+difftastic-sticky--overlay window)))
        (if (<= start (point-min))
            (when ov (delete-overlay ov))
          (unless ov
            (setq ov (make-overlay start start))
            (overlay-put ov '+difftastic-sticky window)
            (overlay-put ov 'window window)
            ;; Above hunk checkboxes at the same spot.
            (overlay-put ov 'priority 1000))
          (move-overlay ov start start)
          (overlay-put ov 'before-string
                       (+difftastic-sticky--text (1- start)
                                                 (1- (window-max-chars-per-line window)))))))))

(defun +difftastic-sticky-update-all (&rest _)
  "Redo the bars in every window that shows this buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (+difftastic-sticky-update window)))

(defun +difftastic-sticky-h ()
  (add-hook 'window-scroll-functions #'+difftastic-sticky-update nil t))
(add-hook 'difftastic-mode-hook #'+difftastic-sticky-h)

;; Redo the bars once the text settles: after difft finishes, after
;; difftastic-groups renders, and after folding.
(define-advice difftastic--run-command-sentinel
    (:after (process &rest _) +difftastic-sticky -95)
  (when (and (eq (process-status process) 'exit)
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (+difftastic-sticky-update-all))))
(advice-add '+difftastic-groups--render :after #'+difftastic-sticky-update-all)
(advice-add 'difftastic-hide-chunk :after #'+difftastic-sticky-update-all)
(advice-add 'difftastic-show-chunk :after #'+difftastic-sticky-update-all)
(advice-add '+difftastic-groups-toggle-section :after #'+difftastic-sticky-update-all)

;;; difftastic-sticky.el ends here
