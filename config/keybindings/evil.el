;;; config/keybindings/evil.el -*- lexical-binding: t; -*-

(map! :desc "Select whole line" :n "l" (kmacro "^ v $ <left>")
      :desc "space" :n "_" (cmd! (insert " ") (evil-normal-state)))

(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(define-and-bind-text-object "-" "---" "---")
(define-and-bind-text-object "v" "$" "$")

;; NOTE: For some reason, "\]" does not work in the regex string. Maybe it needs more escapes?
;;       The second one is the same except for the dot: `.'
(let ((var-string "[[:space:]:\n\"\'\(\)\{\}\[\.\/]"))
  (define-and-bind-text-object "." var-string var-string))
(let ((var-string "[[:space:]:\n\"\'\(\)\{\}\[\/]"))
  (define-and-bind-text-object "r" var-string var-string))

(evil-define-operator bah/evil-duplicate (beg end type register)
  "Duplicate the text from BEG to END, preserving TYPE.
TYPE is one of `char', `line', or `block'. REGISTER is ignored here."
  (let* ((text (buffer-substring-no-properties beg end))
         (to-insert (if (eq type 'line)
                        (concat text (if (string-suffix-p "\n" text)
                                         ""
                                       "\n"))
                      text)))
    (save-excursion
      (goto-char end)
      (insert to-insert))))

(map! :desc "Duplicate text" :nv "h" #'bah/evil-duplicate)

