;; -*- lexical-binding: t; -*-

(defun bah/read-unicode-char (c1 c2 c3 c4 _trailing_space_ignored)
  "Convert unicode input C1 C2 C3 C4 to the corresponding insert char call."
  (interactive "c\nc\nc\nc\nc")
  (insert-char (string-to-number (format "%c%c%c%c" c1 c2 c3 c4) 16)))

(map! :i "C-u" nil)
(map! "C-S-u" #'bah/read-unicode-char)

;; ↑ This sometimes breaks when `ibus' does something weird.
;; For that, an `ibus' restart is sufficient
(map! :leader
      :prefix ("e" . "bah")
      :desc "Restart ibus" :nv "i" (cmd! (shell-command "ibus restart &")))

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times.
Does not add to kill ring."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(map! :i "C-f" #'kill-word
      :i "C-<backspace>" #'doom/delete-backward-word
      :i "C-<delete>" #'delete-word)

(map! :nv "M-." (cmd! (affe-find "~")))

(map! :leader
      :desc "Restart Workspace" "c R" #'lsp-workspace-restart
      :desc "Show full error" "c h" #'flycheck-copy-errors-as-kill)

(map! :leader
      :desc "+format-buffer" "b f" #'+format/buffer)

(map! :nv
      :desc "Add new snippet" "C-S-y" #'+snippets/new
      :desc "Find snippet" "C-." #'+snippets/find-for-current-mode)


(defun bah/split-parent-window-right (&optional size)
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

(map! :leader
      :desc "Add a parent right split to current window" :nv "w e" #'bah/split-parent-window-right
      :desc "vsplit window" :nv "w t" #'evil-window-vsplit
      :nv "w v" #'evil-window-top-left)

(map! :ni
      "S-<left>"  #'evil-window-left
      "S-<down>"  #'evil-window-down
      "S-<up>"    #'evil-window-up
      "S-<right>" #'evil-window-right

      "C-S-<left>"       #'+evil/window-move-left
      "C-S-<down>"       #'+evil/window-move-down
      "C-S-<up>"         #'+evil/window-move-up
      "C-S-<right>"      #'+evil/window-move-right)

(map! :leader
      :desc "direnv reload" :nv "e d" #'envrc-reload

      :desc "LSP: Format buffer" :nv "c f" #'lsp-format-buffer
      :desc "Toggle line wrapping" :v "v" #'visual-line-mode
      :desc "Copy file to clipboard" :nv "y" (cmd! (evil-ex-execute "%y+")))

(map! :nv "M-d" #'evil-mc-make-and-goto-next-match
      :nv "M-v" #'evil-mc-skip-and-goto-next-match)

;; sexp editing/movement
(defun bah/evil-eol-advice (&optional _count)
  (when (evil-eolp)
    (forward-char)))

(advice-add 'sp-forward-sexp :before #'bah/evil-eol-advice)

(map! "C-(" #'sp-backward-slurp-sexp
      "C-)" #'sp-forward-slurp-sexp
      "C-{" #'sp-backward-barf-sexp
      "C-}" #'sp-forward-barf-sexp

      "M-m" #'sp-raise-sexp
      "M-h" #'sp-absorb-sexp
      "M-k" #'sp-transpose-sexp

      "M-l" #'sp-beginning-of-sexp
      "M-'" #'sp-end-of-sexp

      "M-n" #'sp-backward-sexp
      "M-i" #'sp-down-sexp
      "M-e" #'sp-backward-up-sexp
      "M-o" #'sp-forward-sexp
      "M-u" #'sp-backward-down-sexp
      "M-y" #'sp-up-sexp)
