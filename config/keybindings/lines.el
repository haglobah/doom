;;; config/keybindings/lines.el -*- lexical-binding: t; -*-

(defun bah/duplicate-line-or-region-up (arg)
  "Duplicate the current line or selected region upward."
  (interactive "*p")
  (dotimes (_ arg)
    (bah/duplicate-line-or-region -1)))

(defun bah/duplicate-line-or-region-down (arg)
  "Duplicate the current line or selected region downward."
  (interactive "*p")
  (dotimes (_ arg)
    (bah/duplicate-line-or-region 1)))

(defun bah/duplicate-line-or-region (n)
  "Duplicate the current line or selected region upward or downward. The
direction is determined by the argument n being positive (i.e. downward)
or negative (i.e. upward). When a region is in play, the region will be kept
in a way so that this duplicate command can be replayed multiple times."
  (let* ((is-region (use-region-p))
         (beg (if is-region (region-beginning) nil))
         (end (if is-region (region-end) nil))
         (start (point))
         (region-other-point (if is-region (if (eq start beg) end beg)))
         (line-start (if is-region
                         (save-mark-and-excursion
                           (goto-char beg)
                           (line-beginning-position))
                       (line-beginning-position)))
         (line-end (if is-region
                       (save-mark-and-excursion
                         (goto-char end)
                         (line-end-position))
                     (line-end-position)))
         (text (concat (buffer-substring line-start line-end) "\n"))
         (forward (if (< 0 n) 1 -1))
         (shift (length text)))
    ;; If duplication direction is down (forward), keep the existing lines as
    ;; they appear, and insert before the current line. This makes it look as
    ;; if the selection has moved down, and can be used to keep duplicating
    ;; the selected lines downwards.
    (if (> forward 0)
        ;; Because other content will be inserted, save-mark-and-excursion won't
        ;; work. For that reason, I need to do some manual mark adjustment.
        (progn (goto-char line-start)
               (insert text)
               (goto-char (+ start shift))
               (when is-region
                 (save-excursion
                   (push-mark (+ region-other-point shift) 'nomsg nil))))

      ;; If duplication direction is up, insert the item below the current
      ;; line. This allows upward duplication to be repeated.
      (progn (goto-char line-end)
             (forward-line 1)
             (insert text)
             (goto-char start)
             (when is-region
               (save-excursion
                 (push-mark region-other-point 'nomsg nil)))))

    (setq deactivate-mark nil)))

(defun bah/move-line-or-region (arg)
  "Move the current line or region up or down by ARG lines.
With a selected region, move all lines that are
wholly or partially within the region.
Negative ARG moves up, positive ARG moves down."
  (interactive "*p")
  (let ((beg)
        (end)
        (region-active (use-region-p)))
    ;; Set up the range to move
    (if region-active
        (setq beg (region-beginning)
              end (region-end))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))

    ;; Move the text
    (let ((col (current-column))
          (line-offset (if (< arg 0) -1 1)))
      (cond
       ;; Moving up but at buffer start
       ((and (< arg 0) (= beg (point-min)))
        (user-error "Beginning of buffer"))
       ;; Moving down but at buffer end
       ((and (> arg 0) (= end (point-max)))
        (user-error "End of buffer"))
       ;; Otherwise proceed
       (t
        (save-excursion
          (goto-char (if (< arg 0) beg end))
          (forward-line line-offset)
          (when (and (> arg 0) (eobp))
            (newline))
          (let ((text (delete-and-extract-region beg end)))
            (insert text)))
        (forward-line (* line-offset (if region-active
                                         (count-lines beg end)
                                       1)))
        (move-to-column col))))))

(map! "M-<up>" (cmd! (bah/move-line-or-region -1))
      "M-<down>" (cmd! (bah/move-line-or-region 1))
      "M-S-<up>" #'bah/duplicate-line-or-region-up
      "M-S-<down>" #'bah/duplicate-line-or-region-down)
