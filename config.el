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

(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 16 :weight 'normal))
(setq doom-font-increment 1)

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
      :desc "Change to framework font size" :g "!" (cmd! (doom/increase-font-size 4))
      :desc "Reset UI font size" :g "=" #'doom/reset-font-size)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq catppuccin-flavor 'mocha)
(setq doom-theme 'catppuccin)
(setq rainbow-delimiters-max-face-count 9)

(custom-set-variables
 '(display-battery-mode +1)
 '(battery-load-low 30)
 '(battery-load-critical 15))

(setq doom-localleader-key ",")

(load! "config/keybindings.el")
(load! "config/keybindings/evil.el")
(load! "config/keybindings/lines.el")
(load! "config/keybindings/garden.el")
(load! "config/keybindings/elixir.el")
(load! "config/keybindings/workspaces.el")
(load! "config/keybindings/links.el")
(load! "config/keybindings/files.el")

(load! "config/lsp-mode.el")
(load! "config/programming-language-specifics.el")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq which-key-idle-delay 0.2)
(setq shell-file-name (executable-find "bash"))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

(setq auto-save-visited-interval 0.5)
(auto-save-visited-mode t)
(global-auto-revert-mode t)
(setq! doom-modeline-buffer-modification-icon nil)

(setq! doom-modeline-vcs-max-length 40)

(setq projectile-enable-caching nil)

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
(load! "config/mycelium.el")
(load! "config/avy.el")
(load! "config/company.el")

(load! "packages/monkeytype.el")
(load! "packages/bluesky.el")
(load! "packages/eat.el")
(load! "packages/why-this.el")

(load! "packages/gptel.el")
(load! "packages/aidermacs.el")
(load! "packages/agent-shell.el")

(load! "packages/rescript.el")
(load! "packages/gleam.el")
(load! "packages/purescript.el")
(load! "packages/just.el")

;; (global-obsidian-mode t)
;; (load! "packages/obsidian.el")

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
