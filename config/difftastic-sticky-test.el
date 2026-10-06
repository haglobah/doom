;;; difftastic-sticky-test.el --- Tests for difftastic-sticky.el -*- lexical-binding: t; -*-
;;; Run via: just test-difftastic

(require 'ert)
(require 'difftastic)
(defvar +difftastic-groups--steps nil)
(defvar +difftastic-groups--view nil)
(dolist (file '("difftastic-stats.el" "difftastic-sticky.el"))
  (load (expand-file-name file (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(defconst difftastic-sticky-test--text
  (concat "Commit message\n"
          "\n"
          "a.el --- 1/2 --- Emacs Lisp\n"
          "1 (foo)\n"
          "2 (bar)\n"
          "\n"
          "a.el --- 2/2 --- Emacs Lisp\n"
          "10 (baz)\n"
          "\n"
          "b.py --- Python\n"
          "1 x = 1\n"
          "2 y = 2\n"))

(defmacro difftastic-sticky-test--with-buffer (&rest body)
  "Run BODY in a difftastic buffer shown in the selected window."
  `(with-temp-buffer
     (insert difftastic-sticky-test--text)
     (difftastic-mode)
     (set-window-buffer nil (current-buffer))
     ,@body))

(defun difftastic-sticky-test--pos (text)
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (match-beginning 0)))

(defun difftastic-sticky-test--shown (start)
  "The sticky text after scrolling the window to START, or nil."
  (set-window-start nil start)
  (+difftastic-sticky-update)
  (when-let* ((ov (seq-find (lambda (ov) (overlay-get ov '+difftastic-sticky))
                            (overlays-in (point-min) (point-max)))))
    (should (eq (overlay-get ov 'window) (selected-window)))
    (should (= (overlay-start ov) start))
    (substring-no-properties (overlay-get ov 'before-string))))

(ert-deftest difftastic-sticky-nothing-before-anything-scrolls-off ()
  (difftastic-sticky-test--with-buffer
   (should-not (difftastic-sticky-test--shown (point-min)))))

(ert-deftest difftastic-sticky-shows-the-file-scrolled-under-it ()
  (difftastic-sticky-test--with-buffer
   (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "1 (foo)"))
                  " a.el\n"))
   (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "2 y = 2"))
                  " b.py\n"))))

(ert-deftest difftastic-sticky-keeps-the-old-file-until-the-next-header-passes ()
  (difftastic-sticky-test--with-buffer
   ;; The next file's own header sits right under the bar: the bar still
   ;; names the file above it, and gives way once the header scrolls off.
   (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "b.py --- "))
                  " a.el\n"))
   (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "1 x = 1"))
                  " b.py\n"))))

(ert-deftest difftastic-sticky-blank-above-the-first-file ()
  (difftastic-sticky-test--with-buffer
   (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "\na.el"))
                  " \n"))))

(ert-deftest difftastic-sticky-shows-counts-from-stats ()
  (difftastic-sticky-test--with-buffer
   (setq-local +difftastic-stats--entries '(("a.el" 3 1) ("b.py" nil nil)))
   (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "1 (foo)"))
                  " a.el  +3 −1\n"))
   (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "2 y = 2"))
                  " b.py  binary\n"))))

(ert-deftest difftastic-sticky-adds-the-part-once-groups-exist ()
  (difftastic-sticky-test--with-buffer
   (let ((core '(:label "Core change" :summary "Adds the parser."))
         (tests '(:label "Tests")))
     (setq-local +difftastic-groups--view (list :groups (list core tests)))
     (setq-local +difftastic-groups--steps
                 (list (cons core (copy-marker (difftastic-sticky-test--pos "a.el --- 1/2")))
                       (cons tests (copy-marker (difftastic-sticky-test--pos "b.py --- ")))))
     (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "1 (foo)"))
                    " 1/2 Core change — Adds the parser.\n a.el\n"))
     (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "2 y = 2"))
                    " 2/2 Tests\n b.py\n"))
     ;; Above the first group the part line stays, blank, so the bar
     ;; keeps its height.
     (should (equal (difftastic-sticky-test--shown (difftastic-sticky-test--pos "\na.el"))
                    " \n \n")))))

(ert-deftest difftastic-sticky-bars-hide-the-tint-beneath ()
  ;; Every character ends its faces with `default', so the changed line
  ;; under the bars can't lend them its background; the bar face and the
  ;; path's bold stay in front of it.
  (difftastic-sticky-test--with-buffer
   (setq-local +difftastic-stats--entries '(("a.el" 3 1)))
   (difftastic-sticky-test--shown (difftastic-sticky-test--pos "1 (foo)"))
   (let ((text (overlay-get (seq-find (lambda (ov) (overlay-get ov '+difftastic-sticky))
                                      (overlays-in (point-min) (point-max)))
                            'before-string)))
     (dotimes (i (length text))
       (let ((faces (ensure-list (get-text-property i 'face text))))
         (should (eq (car (last faces)) 'default))
         (should (memq '+difftastic-hunk-header faces))))
     (should (memq 'bold (ensure-list (get-text-property 1 'face text)))))))

(ert-deftest difftastic-sticky-follows-each-window-separately ()
  (difftastic-sticky-test--with-buffer
   (let ((other (split-window)))
     (unwind-protect
         (progn
           (set-window-buffer other (current-buffer))
           (difftastic-sticky-test--shown (difftastic-sticky-test--pos "1 (foo)"))
           (with-selected-window other
             (set-window-start nil (difftastic-sticky-test--pos "2 y = 2"))
             (+difftastic-sticky-update))
           (let ((ovs (seq-filter (lambda (ov) (overlay-get ov '+difftastic-sticky))
                                  (overlays-in (point-min) (point-max)))))
             (should (= (length ovs) 2))
             (should (equal (sort (mapcar (lambda (ov) (substring-no-properties
                                                        (overlay-get ov 'before-string)))
                                          ovs)
                                  #'string<)
                            '(" a.el\n" " b.py\n")))))
       (delete-window other)))))

;;; difftastic-sticky-test.el ends here
