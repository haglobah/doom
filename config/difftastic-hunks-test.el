;;; difftastic-hunks-test.el --- Tests for difftastic-hunks.el -*- lexical-binding: t; -*-
;;; Run via: just test-difftastic

(require 'ert)
(require 'difftastic)
(load (expand-file-name "difftastic-hunks.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst difftastic-hunks-test--text
  (concat "a.el --- 1/2 --- Emacs Lisp\n"
          "1 (foo)\n"
          "2 (bar)\n"
          "\n"
          "a.el --- 2/2 --- Emacs Lisp\n"
          "10 (baz)\n"
          "\n"
          "b.py --- Python\n"
          "1 x = 1\n"))

(defmacro difftastic-hunks-test--with-buffer (&rest body)
  "Run BODY in a decorated difftastic buffer holding the test hunks."
  `(with-temp-buffer
     (insert difftastic-hunks-test--text)
     (difftastic-mode)
     (+difftastic-hunks-decorate)
     (goto-char (point-min))
     ,@body))

(defun difftastic-hunks-test--goto (text)
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(defun difftastic-hunks-test--boxes ()
  "Each header's checkbox string, in buffer order."
  (mapcar (lambda (ov) (substring-no-properties (overlay-get ov 'before-string)))
          (sort (seq-filter (lambda (ov) (overlay-get ov 'before-string))
                            (overlays-in (point-min) (point-max)))
                (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun difftastic-hunks-test--face-at (pos)
  (seq-some (lambda (ov) (and (eq (overlay-get ov '+difftastic-hunks) 'header)
                              (overlay-get ov 'face)))
            (overlays-at pos)))

(ert-deftest difftastic-hunks-every-header-gets-a-gray-bar-and-a-box ()
  (difftastic-hunks-test--with-buffer
   (should (equal (difftastic-hunks-test--boxes) '("☐ " "☐ " "☐ ")))
   (difftastic-hunks-test--goto "a.el --- 2/2")
   (should (eq (difftastic-hunks-test--face-at (point)) '+difftastic-hunk-header))
   ;; Neither the body nor the blank line before a header.
   (should-not (difftastic-hunks-test--face-at (1- (point))))
   (difftastic-hunks-test--goto "10 (baz)")
   (should-not (difftastic-hunks-test--face-at (point)))))

(ert-deftest difftastic-hunks-checked-header-colors-the-line-counts ()
  (difftastic-hunks-test--with-buffer
   (difftastic-hunks-test--goto "b.py --- ")
   (let ((stats (make-overlay (pos-eol) (pos-eol))))
     (overlay-put stats '+difftastic-stats t)
     (overlay-put stats 'after-string (propertize "  +1 −0" 'face 'bold))
     (+difftastic-hunks-toggle-checked)
     (let ((s (overlay-get stats 'after-string)))
       (should (equal (substring-no-properties s) "  +1 −0"))
       (should (equal (get-text-property 3 'face s)
                      '(bold +difftastic-hunk-header-checked))))
     (+difftastic-hunks-toggle-checked)
     (should (equal (get-text-property 3 'face (overlay-get stats 'after-string))
                    '(bold +difftastic-hunk-header))))))

(ert-deftest difftastic-hunks-check-from-the-body-and-uncheck ()
  (difftastic-hunks-test--with-buffer
   (difftastic-hunks-test--goto "(baz)")
   (+difftastic-hunks-toggle-checked)
   (should (equal (difftastic-hunks-test--boxes) '("☐ " "☑ " "☐ ")))
   (difftastic-hunks-test--goto "a.el --- 2/2")
   (should (eq (difftastic-hunks-test--face-at (point)) '+difftastic-hunk-header-checked))
   ;; Point stays where it was; again from the header unchecks.
   (+difftastic-hunks-toggle-checked)
   (should (equal (difftastic-hunks-test--boxes) '("☐ " "☐ " "☐ ")))))

(ert-deftest difftastic-hunks-check-outside-a-hunk-fails ()
  (with-temp-buffer
    (insert "commit abc\n\n" difftastic-hunks-test--text)
    (difftastic-mode)
    (goto-char (point-min))
    (should-error (+difftastic-hunks-toggle-checked) :type 'user-error)))

(ert-deftest difftastic-hunks-checks-follow-content-not-position ()
  (difftastic-hunks-test--with-buffer
   (difftastic-hunks-test--goto "x = 1")
   (+difftastic-hunks-toggle-checked)
   ;; Moving the file to the top (as difftastic-groups reorders) keeps it.
   (let ((inhibit-read-only t))
     (erase-buffer)
     (insert "b.py --- Python\n1 x = 1\n\n"
             (substring difftastic-hunks-test--text 0
                        (string-search "b.py" difftastic-hunks-test--text))))
   (+difftastic-hunks-decorate)
   (should (equal (difftastic-hunks-test--boxes) '("☑ " "☐ " "☐ ")))
   ;; A changed hunk under the same header is unchecked again.
   (let ((inhibit-read-only t))
     (difftastic-hunks-test--goto "x = 1")
     (replace-match "x = 2"))
   (+difftastic-hunks-decorate)
   (should (equal (difftastic-hunks-test--boxes) '("☐ " "☐ " "☐ ")))))

(ert-deftest difftastic-hunks-tab-in-the-body-folds-its-hunk ()
  (difftastic-hunks-test--with-buffer
   (difftastic-hunks-test--goto "(bar)")
   (+difftastic-hunks-toggle)
   (should (looking-at-p "a.el --- 1/2"))
   (should (memq :hidden (get-text-property (point) 'difftastic)))
   (should (invisible-p (save-excursion (search-forward "(bar)") (match-beginning 0))))
   (should-not (invisible-p (save-excursion (search-forward "(baz)") (match-beginning 0))))
   ;; The bar reaches the window edge: the newline ending the
   ;; folded display line (difft's fold keeps the body's last newline
   ;; visible) carries the header face too.
   (let ((nl (save-excursion (search-forward "(bar)"))))
     (should-not (invisible-p nl))
     (should (eq (difftastic-hunks-test--face-at nl) '+difftastic-hunk-header)))
   ;; TAB on the header unfolds again, and the extra bar goes away.
   (+difftastic-hunks-toggle)
   (should-not (invisible-p (save-excursion (search-forward "(bar)") (match-beginning 0))))
   (let ((nl (save-excursion (search-forward "(bar)"))))
     (should-not (difftastic-hunks-test--face-at nl)))))

(ert-deftest difftastic-hunks-tab-outside-a-hunk-fails ()
  (with-temp-buffer
    (insert "commit abc\n\n" difftastic-hunks-test--text)
    (difftastic-mode)
    (goto-char (point-min))
    (should-error (+difftastic-hunks-toggle) :type 'user-error)))

;;; difftastic-hunks-test.el ends here
