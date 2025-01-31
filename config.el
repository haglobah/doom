;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Source Code Pro" :size 16 :weight 'medium))
(setq doom-font-increment 1)

;;      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 18))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(map! :leader
      :desc "Decrease UI font size" :g "-" #'doom/decrease-font-size
      :desc "Increase UI font size" :g "+" #'doom/increase-font-size
      :desc "Reset UI font size" :g "=" #'doom/reset-font-size)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq catppuccin-flavor 'macchiato)
(setq doom-theme 'catppuccin)
(setq rainbow-delimiters-max-face-count 9)

(setq doom-localleader-key ",")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq which-key-idle-delay 0.2)
(setq shell-file-name (executable-find "bash"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq auto-save-visited-interval 0.1)
(auto-save-visited-mode t)

(global-auto-revert-mode t)

(map! :i "C-u" nil)

(defun read-unicode-char (c1 c2 c3 c4 _trailing_space_ignored)
  "Convert unicode input C1 C2 C3 C4 to the corresponding insert char call."
  (interactive "c\nc\nc\nc\nc")
  (insert-char (string-to-number (format "%c%c%c%c" c1 c2 c3 c4) 16)))

(map! "C-S-u" 'read-unicode-char)

(map! :nv "M-." (lambda () (interactive) (affe-find "~")))

(setq projectile-enable-caching nil)

(map! :ni
      "S-<left>"  #'evil-window-left
      "S-<down>"  #'evil-window-down
      "S-<up>"    #'evil-window-up
      "S-<right>" #'evil-window-right

      "C-S-<left>"       #'+evil/window-move-left
      "C-S-<down>"       #'+evil/window-move-down
      "C-S-<up>"         #'+evil/window-move-up
      "C-S-<right>"      #'+evil/window-move-right)

(map! :leader :desc "Toggle line wrapping" :v "v" #'visual-line-mode)
(map! :leader :desc "Copy file to clipboard" :nv "y" (cmd! (evil-ex-execute "%y+")))

(after! magit
  (add-hook 'magit-mode-hook #'evil-emacs-state))

;; why-this
(use-package! why-this
  :defer

  :init
  (map! :leader :desc "Toggle inline git blame" :nv "b w" #'why-this-mode)

  :config
  (setq! why-this-idle-delay 0.01)
  (set-face-attribute 'why-this-face nil :foreground "gray" :slant 'oblique))

(defun split-parent-window-right (&optional size)
  "Split the parent window into two side-by-side windows.
If there is no parent, splits the current window. Otherwise
identical to `split-window-right'."
  (interactive "P")
  (let ((old-window (selected-window))
        (size (and size (prefix-numeric-value size)))
        new-window)
    (when (and size (< size 0) (< (- size) window-min-width))
      ;; `split-window' would not signal an error here.
      (error "Size of new window too small"))
    (setq new-window (split-window (window-parent) size t))
    ;; Always copy quit-restore parameter in interactive use.
    (let ((quit-restore (window-parameter old-window 'quit-restore)))
      (when quit-restore
        (set-window-parameter new-window 'quit-restore quit-restore)))
    new-window))

(map! :leader :desc "Add a parent right split to current window" :nv "w e" #'split-parent-window-right)

(map! "C-(" #'sp-backward-slurp-sexp
      "C-)" #'sp-forward-slurp-sexp
      "C-{" #'sp-backward-barf-sexp
      "C-}" #'sp-forward-barf-sexp)

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

(defun bah/move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun bah/move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(map! "M-<up>" #'bah/move-line-up
      "M-<down>" #'bah/move-line-down
      "M-S-<up>" #'bah/duplicate-line-or-region-up
      "M-S-<down>" #'bah/duplicate-line-or-region-down)

(map! :leader :desc "LSP: Format buffer" :nv "c f" #'lsp-format-buffer)

;; vim
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
(let ((var-string "[[:space:],\n\"\'\(\)\{\}\[]"))
  (define-and-bind-text-object "v" var-string var-string))

(map! :n "_" (cmd! (insert " ") (evil-normal-state)))

(setq! doom-modeline-buffer-modification-icon nil)

(ws-butler-global-mode nil)

;; aider

;; (use-package! aider
;;   :config
;;   ;; (setq! aider-args '("--model" "gpt-4o-mini"))
;;   (setq! aider-args '("--model" "o1-mini")))


;; treemacs
;; (setq! treemacs-file-event-delay 100)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
