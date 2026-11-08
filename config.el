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

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 20 :weight 'normal))
(setq doom-font-increment 1)

;; Doom maps the Nerd Font private-use ranges to "Symbols Nerd Font Mono". At
;; this size its glyphs advance 37px in a 23px cell, so every icon in a
;; terminal (eat, starship prompt) shifts the rest of the line by 0.6 cells.
;; FiraCode Nerd Font carries the same glyphs at exactly one cell.
(defun +beat/nerd-icons-one-cell-h ()
  "Render Nerd Font private-use glyphs with the default font, one cell wide."
  (dolist (range '((#xe000 . #xf8ff) (#xf0000 . #xfffff)))
    (set-fontset-font t range (font-spec :family "FiraCode Nerd Font"))))
(add-hook 'after-setting-font-hook #'+beat/nerd-icons-one-cell-h)

;; (setq debug-on-error t)

(setq read-process-output-max (* 3 1024 1024)) ;; 3mb
(add-to-list 'default-frame-alist '(undecorated . t))

;;      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 18))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

(map! :leader
      :desc "Decrease UI font size" :g "-" #'doom/decrease-font-size
      :desc "Increase UI font size" :g "+" #'doom/increase-font-size
      :desc "Super-decrease UI font size" :g "@" (cmd! (doom/decrease-font-size 4))
      :desc "Super-increase UI font size" :g "!" (cmd! (doom/increase-font-size 4))
      :desc "Reset UI font size" :g "=" #'doom/reset-font-size)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq catppuccin-flavor 'mocha)
(setq doom-theme 'catppuccin)
(setq rainbow-delimiters-max-face-count 9)

(setq battery-load-low 30
      battery-load-critical 15)
(display-battery-mode +1)

(setq doom-localleader-key ",")

;; utils.el first: keybindings/links.el calls `bah/i' at runtime.
(load! "config/utils.el")

(load! "config/keybindings.el")
(load! "config/keybindings/evil.el")
(load! "config/keybindings/lines.el")
(load! "config/keybindings/garden.el")
(load! "config/keybindings/elixir.el")
(load! "config/keybindings/workspaces.el")
(load! "config/keybindings/links.el")
(load! "config/keybindings/files.el")
(load! "config/keybindings/markdown.el")
(load! "config/keybindings/fetch-link-title.el")
(load! "config/keybindings/surround.el")

(load! "config/lsp-mode.el")
(load! "config/programming-language-specifics.el")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq which-key-idle-delay 0.2)
(after! which-key
  ;; Ctrl-prefixes are safe globally — they don't conflict with text input.
  ;; `which-key-mode' builds its keymap from `which-key-paging-prefixes' once at
  ;; load time, so we re-install the bindings here after our `setq' takes effect.
  (setq which-key-paging-prefixes '("C-c" "C-x" "C-h"))
  (dolist (prefix which-key-paging-prefixes)
    (define-key which-key-mode-map
                (kbd (concat prefix " " which-key-paging-key))
                #'which-key-C-h-dispatch)))

;; Leader paging: only reached via SPC in evil normal/motion/visual states, so
;; plain SPC stays usable in insert-state buffers (e.g. magit commit messages).
(map! :leader "<f5>" #'which-key-C-h-dispatch)
(setq shell-file-name (executable-find "bash"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Save-on-switch/focus/idle lives in packages/super-save.el.
;; NOTE: saving on `evil-insert-state-exit-hook' breaks `dd.` in vim — don't.
(load! "packages/super-save.el")
;; This makes buffers not diverge from files
(global-auto-revert-mode t)
(setq! doom-modeline-buffer-modification-icon nil)

(setq! doom-modeline-vcs-max-length 40)

(setq projectile-enable-caching nil)

;; Increase the number of files in the recent file list
(setq recentf-max-saved-items 500)

(setq whitespace-line-column 100)
(setq whitespace-global-modes
      '(not magit-status-mode
        org-mode))
(setq whitespace-style
      '(face missing-newline-at-eof
        trailing empty tabs tab-mark))
(setq whitespace-display-mappings
      '((tab-mark 9
         [187 9]
         [92 9])))
(global-whitespace-mode +1)
(global-visual-line-mode +1)

(load! "config/email.el")
(load! "config/magit.el")
(load! "packages/majutsu.el")
(load! "config/mycelium.el")
(load! "config/section-word-count.el")
(load! "config/avy.el")
(load! "config/company.el")
(load! "config/apheleia.el")

(load! "config/completion/markdown.el")

(load! "packages/plz.el")
(load! "packages/monkeytype.el")
(load! "packages/writegood.el")
(add-hook! '(text-mode-hook markdown-mode-hook org-mode-hook) #'writegood-mode)
(map! "C-c g" #'writegood-mode)
(load! "packages/bluesky.el")
(load! "packages/eat.el")
(load! "packages/why-this.el")

;; (load! "packages/gptel.el")
(load! "packages/aidermacs.el")
(load! "packages/agent-shell.el")

(load! "packages/racket.el")
(load! "packages/rescript.el")
(load! "packages/gleam.el")
(load! "packages/purescript.el")
(load! "packages/just.el")

;; (global-obsidian-mode t)
;; (load! "packages/obsidian.el")

;; treemacs
;; (setq! treemacs-file-event-delay 100)

;; clojure-ts-mode 0.6 needs a recent tree-sitter-clojure grammar (str_content,
;; old_meta_lit). Point treesit-install-language-grammar at sogaiu's repo.
(after! treesit
  (add-to-list 'treesit-language-source-alist
               '(clojure "https://github.com/sogaiu/tree-sitter-clojure")))

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

;; Opened from the shell (fish alt-o) via emacsclient; see noldor home/programs/fish.nix.
(defun +beat/open-project (root)
  "Switch to the project at ROOT in the current frame and raise it.

Resumes the workspace that belongs to ROOT (matched by its project
directory, else by project name) where it was left. When no such
workspace exists or it holds no real buffer, opens the most recently
visited file under ROOT, or dired at ROOT when recentf knows none.
Called from the shell via emacsclient."
  (let* ((root (file-name-as-directory (file-truename root)))
         (name (let ((default-directory root)) (doom-project-name)))
         (ws (or (seq-find (lambda (p)
                             (let ((dir (persp-parameter '+workspace-project p)))
                               (and dir (file-equal-p dir root))))
                           (+workspace-list))
                 (+workspace-get name t))))
    (projectile-add-known-project root)
    (+workspace-switch (if ws (safe-persp-name ws) name) t)
    (set-persp-parameter '+workspace-project root (+workspace-current))
    (unless (seq-some #'doom-real-buffer-p (+workspace-buffer-list))
      (let ((recent (seq-find (lambda (f) (string-prefix-p root (expand-file-name f)))
                              recentf-list)))
        (if recent
            (find-file recent)
          (dired root))))
    (select-frame-set-input-focus (selected-frame))
    (+workspace-current-name)))
