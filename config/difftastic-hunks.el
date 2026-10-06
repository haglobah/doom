;;; difftastic-hunks.el --- Hunk header bars and checkboxes in difftastic -*- lexical-binding: t; -*-

;; Each hunk header ("a.el --- 2/3 --- Emacs Lisp") gets a gray bar and
;; a checkbox.  Checking a hunk turns its bar green.  A check belongs to the
;; hunk's text, not its position, so it survives difftastic-groups
;; moving files around and `g' rerunning difft, and it drops off when
;; the hunk itself changes.
;; TAB folds the hunk at point from anywhere in it.

(require 'difftastic)

(defface +difftastic-hunk-header-checked '((t :extend t))
  "Header of a checked hunk in difftastic output.")
(defface +difftastic-hunk-header '((t :extend t))
  "Header of a hunk in difftastic output.")

(defun +difftastic-hunks-set-faces (&rest _)
  "Derive the colors from the theme."
  (when (fboundp 'doom-blend)
    (let ((bg (face-background 'default nil t)))
      (set-face-attribute '+difftastic-hunk-header-checked nil
                          :background (doom-blend (face-foreground 'success nil t) bg 0.35))
      (set-face-attribute '+difftastic-hunk-header nil
                          :background (doom-blend (face-foreground 'default nil t) bg 0.12)))))
(+difftastic-hunks-set-faces)
(add-hook 'doom-load-theme-hook #'+difftastic-hunks-set-faces)

(defvar-local +difftastic-hunks--checked nil
  "Keys of the checked hunks, from `+difftastic-hunks--key'.")
;; `difftastic-rerun' calls `difftastic-mode' again; keep the checks.
(put '+difftastic-hunks--checked 'permanent-local t)

(defun +difftastic-hunks--headers ()
  "Positions of the hunk header lines, in buffer order."
  (let (headers)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (difftastic--chunk-regexp nil) nil t)
        (unless (difftastic--point-at-added-removed-p)
          (push (pos-bol) headers))))
    (nreverse headers)))

(defun +difftastic-hunks--key (bol next)
  "Key of the hunk whose header starts at BOL; NEXT ends its body."
  (sha1 (concat (buffer-substring-no-properties bol (pos-eol))
                "\n"
                (string-trim (buffer-substring-no-properties
                              (min (1+ (pos-eol)) next) next)))))

(defun +difftastic-hunks--header-at-point ()
  "Start of the header of the hunk at point, or nil."
  (if (difftastic--point-at-chunk-header-p)
      (pos-bol)
    (difftastic--prev-chunk)))

(defun +difftastic-hunks--visible-eol (eol)
  "Where the display line ending at EOL really ends, past folded text."
  (let ((pos eol))
    (while (and (< pos (point-max)) (invisible-p pos))
      (setq pos (next-single-char-property-change pos 'invisible)))
    pos))

(defun +difftastic-hunks--overlay (beg end kind face &optional box)
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov '+difftastic-hunks kind)
    (overlay-put ov 'evaporate t)
    (overlay-put ov 'face face)
    (when box
      (overlay-put ov 'before-string (propertize box 'face face)))
    ov))

(defun +difftastic-hunks--paint-stats (eol face)
  "Give difftastic-stats' line counts at EOL the background of FACE."
  (dolist (ov (overlays-in eol eol))
    (when (overlay-get ov '+difftastic-stats)
      (let ((orig (or (overlay-get ov '+difftastic-hunks-orig)
                      (overlay-put ov '+difftastic-hunks-orig
                                   (overlay-get ov 'after-string)))))
        (overlay-put ov 'after-string
                     (let ((s (copy-sequence orig)))
                       (add-face-text-property 0 (length s) face t s)
                       s))))))

(defun +difftastic-hunks-decorate (&rest _)
  "Draw the bar and checkbox of every hunk header in this buffer."
  (when (derived-mode-p 'difftastic-mode)
    (remove-overlays (point-min) (point-max) '+difftastic-hunks 'header)
    (save-excursion
      (let ((headers (+difftastic-hunks--headers)))
        (while headers
          (let* ((bol (pop headers))
                 (next (or (car headers) (point-max))))
            (goto-char bol)
            (unless (invisible-p bol)
              (let* ((checked (member (+difftastic-hunks--key bol next)
                                      +difftastic-hunks--checked))
                     (face (if checked
                               '+difftastic-hunk-header-checked
                             '+difftastic-hunk-header))
                     (eol (pos-eol))
                     (end (+difftastic-hunks--visible-eol eol)))
                (+difftastic-hunks--overlay bol (min (1+ eol) (point-max)) 'header face
                                            (if checked "☑ " "☐ "))
                (+difftastic-hunks--paint-stats eol face)
                ;; A folded hunk hides the header's own newline, so the
                ;; bar would stop at the text; extend it from the
                ;; newline that ends the display line instead.
                (when (and (> end eol) (< end (point-max)))
                  (+difftastic-hunks--overlay end (1+ end) 'header face))))))))))

(defun +difftastic-hunks-toggle-checked ()
  "Check the hunk at point, or uncheck it.  Works anywhere in the hunk."
  (interactive nil difftastic-mode)
  (let* ((bol (or (+difftastic-hunks--header-at-point)
                  (user-error "Not in a hunk")))
         (key (save-excursion
                (goto-char bol)
                (+difftastic-hunks--key bol (or (difftastic--next-chunk)
                                                (point-max))))))
    (setq +difftastic-hunks--checked
          (if (member key +difftastic-hunks--checked)
              (delete key +difftastic-hunks--checked)
            (cons key +difftastic-hunks--checked)))
    (+difftastic-hunks-decorate)))

(defun +difftastic-hunks-toggle (&optional file-chunk)
  "Fold or unfold the hunk at point, from its header or its body.
FILE-CHUNK is as in `difftastic-toggle-chunk'."
  (interactive "P" difftastic-mode)
  (goto-char (or (+difftastic-hunks--header-at-point)
                 (user-error "Not in a hunk")))
  (difftastic-toggle-chunk file-chunk))

;; Redraw whenever the text or the folding changes: after difft finishes
;; and after difftastic-groups (depth -50) has reordered the files, after
;; every later groups render, and after folding.
(define-advice difftastic--run-command-sentinel
    (:after (process &rest _) +difftastic-hunks -90)
  (when (and (eq (process-status process) 'exit)
             (buffer-live-p (process-buffer process)))
    (with-current-buffer (process-buffer process)
      (+difftastic-hunks-decorate))))
(advice-add '+difftastic-groups--render :after #'+difftastic-hunks-decorate)
(advice-add 'difftastic-hide-chunk :after #'+difftastic-hunks-decorate)
(advice-add 'difftastic-show-chunk :after #'+difftastic-hunks-decorate)

;;; difftastic-hunks.el ends here
