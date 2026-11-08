;;; packages/eat.el -*- lexical-binding: t; -*-

(setq eat-shell (executable-find "fish")
      eat-kill-buffer-on-exit t)

;; Globalized modes from config.el leak into terminal buffers and break the
;; cell grid: visual-line-mode re-wraps at spaces, emojify swaps emoji for
;; 47px images (two cells). Keep eat buffers plain.
(add-hook! 'eat-mode-hook
  (defun +beat/eat-plain-buffer-h ()
    (visual-line-mode -1)
    (setq-local truncate-lines t)))
(after! emojify
  (add-to-list 'emojify-inhibit-major-modes 'eat-mode))

(map! :leader
      :desc "Toggle other terminal window" "o t" #'eat-project-other-window
      :desc "Toggle terminal window" "o T" #'eat-project)

(map! :gnvi "C-j" #'eat-project-other-window)
(map! :map eat-char-mode-map :nvi "C-j" #'evil-quit)
(map! :map eat-semi-char-mode-map :nvi "C-j" #'evil-quit)
(map! :map eat-mode-map "C-j" #'evil-quit)


;;; Kitty in eat
;; Mirrors the keybindings in ~/noldor/home/programs/kitty.nix. Kitty tabs and
;; windows are Emacs buffers and windows: splits and shift+arrows act on
;; windows, ctrl+shift+arrows are Doom's global `+evil/window-move-*', and
;; buffer switching replaces kitty's tab navigation.

(defun +beat/eat-new-tab (&optional program)
  "Open a new eat buffer in the current directory (kitty ctrl+t).
PROGRAM is a shell command; nil runs `eat-shell'. The new buffer inherits
`default-directory', which eat keeps current through the shell's OSC 7."
  (interactive)
  (eat program t))

(defun +beat/eat-claude-tab ()
  "Open a new tab running Claude Code here (kitty ctrl+alt+c)."
  (interactive)
  (+beat/eat-new-tab "fish --interactive --init-command 'direnv exec . claude'"))

(defun +beat/eat-split-right ()
  "Open a new eat buffer in a window to the right (kitty ctrl+])."
  (interactive)
  (select-window (split-window-right))
  (+beat/eat-new-tab))

(defun +beat/eat-split-below ()
  "Open a new eat buffer in a window below (kitty ctrl+[)."
  (interactive)
  (select-window (split-window-below))
  (+beat/eat-new-tab))

(defun +beat/eat-send-key (key)
  "Send KEY, a `kbd' string, to the terminal."
  (eat-self-input 1 (aref (kbd key) 0)))

(defun +beat/eat-send-string (string)
  "Send STRING to the terminal as typed input."
  (eat-term-send-string eat-terminal string))

(defun +beat/eat-scroll-mode ()
  "Browse the scrollback with evil normal-state keys (kitty ctrl+shift+space).
Escape or q returns to the terminal."
  (interactive)
  (evil-normal-state))

(defun +beat/eat-scroll-mode-exit ()
  "Return to the terminal at its cursor."
  (interactive)
  (goto-char (eat-term-display-cursor eat-terminal))
  (evil-insert-state))

(map! :map eat-mode-map
      ;; New shells
      :ni "C-t"         #'+beat/eat-new-tab
      :ni "C-M-c"       #'+beat/eat-claude-tab
      ;; Splits and windows. C-[ is Escape in Emacs, so both splits take the
      ;; shifted keys.
      :ni "C-}"         #'+beat/eat-split-right
      :ni "C-{"         #'+beat/eat-split-below
      :ni "S-<left>"    #'windmove-left
      :ni "S-<right>"   #'windmove-right
      :ni "S-<up>"      #'windmove-up
      :ni "S-<down>"    #'windmove-down
      ;; Font size, frame-wide like kitty's `all'
      :ni "C-+"         #'doom/increase-font-size
      :ni "C--"         #'doom/decrease-font-size
      :ni "C-="         #'doom/reset-font-size
      ;; Keys kitty rewrites for fish and TUIs
      :i "C-s"          (cmd! (+beat/eat-send-key "C-w"))
      :i "C-g"          (cmd! (+beat/eat-send-key "M-d"))
      :i "M-h"          (cmd! (+beat/eat-send-key "C-M-d"))
      :i "M-,"          (cmd! (+beat/eat-send-key "C-M-u"))
      ;; Claude Code
      :i "C-M-l"        (cmd! (+beat/eat-send-string "\C-l/clear\r"))
      :i "C-M-r"        (cmd! (+beat/eat-send-string "\C-l/resume\r"))
      ;; Scrollback
      :ni "C-h"         #'bah/scroll-down-half-page
      :ni "C-,"         #'bah/scroll-up-half-page
      :i "C-S-SPC"      #'+beat/eat-scroll-mode
      :n "u"            #'evil-scroll-up
      :n "l"            #'evil-scroll-down
      :n "q"            #'+beat/eat-scroll-mode-exit
      :n "<escape>"     #'+beat/eat-scroll-mode-exit)
