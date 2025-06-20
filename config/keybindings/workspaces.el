;;; config/keybindings/workspaces.el -*- lexical-binding: t; -*-

(defvar bah/persistent-workspaces
  '(("doom" "~/.config/doom/" "config.el")
    ("nix-home" "~/nix-home/" "home.nix")
    ("mynix" "~/mynix/" "configuration.nix")
    ("beathagenlocher.com" "~/beathagenlocher.com/" "flake.nix")
    ("mycelium" "~/mycelium/" "acc.md")
    ("zmk-config-tzcl" "~/projects/zmk-config-tzcl/" "config/rae_dux.keymap")
    ("templater" "~/projects/templater/" "templates/flake.nix")))

(defvar bah/all-workspaces
  (append
   bah/persistent-workspaces
   '(("fabresearcher" "~/ag/fabresearcher/work/" "Justfile")
     ("queue" "~/projects/rescript_instant_todo/" "flake.nix"))))

(cl-defun bah/get-workspace (name workspaces)
  (interactive)
  (dolist (name+dir+file workspaces)
    (if (string-equal name (first name+dir+file))
        (cl-return-from bah/get-workspace name+dir+file)
      nil)))

(defun bah/setup-workspace (name+dir+file)
  (interactive)
  (persp-add-new (first name+dir+file))
  (persp-switch (first name+dir+file))
  (find-file (concat (second name+dir+file) (caddr name+dir+file)))
  (find-file (concat (second name+dir+file) (second (projectile-recentf-files)))))

(defun bah/setup-workspaces (workspaces)
  (interactive)
  (dolist (name+dir+file workspaces)
    (bah/setup-workspace name+dir+file)))

(defun bah/create|switch (workspace-name)
  (interactive)
  (if (+workspace-exists-p workspace-name)
      (+workspace-switch workspace-name)
    (bah/setup-workspace (bah/get-workspace workspace-name bah/all-workspaces))))

(map! :leader
      :desc "Setup Workspaces"    "TAB w" (cmd! (bah/setup-workspaces bah/persistent-workspaces))
      :desc "Delete Workspace"    "TAB x" #'+workspace/kill
      :desc "New Workspace"       "TAB N" #'+workspace/new

      :desc "Launch kitty tab"    "TAB ," (cmd! (shell-command (concat "kitty @ --to=unix:@mykitty launch --type=tab --cwd="
                                                                       (projectile-project-root (file-name-directory (buffer-file-name)))
                                                                       " fish")))

      :desc "doom"                "TAB d" (cmd! (bah/create|switch "doom"))
      :desc "nix-home"            "TAB n" (cmd! (bah/create|switch "nix-home"))
      :desc "mynix"               "TAB m" (cmd! (bah/create|switch "mynix"))
      :desc "beathagenlocher.com" "TAB b" (cmd! (bah/create|switch "beathagenlocher.com"))
      :desc "mycelium"            "TAB o" (cmd! (bah/create|switch "mycelium"))
      :desc "templater"           "TAB t" (cmd! (bah/create|switch "templater"))
      :desc "zmk-config-tzcl"     "TAB z" (cmd! (bah/create|switch "zmk-config-tzcl"))

      :desc "fabresearcher"       "TAB f" (cmd! (bah/create|switch "fabresearcher"))
      :desc "queue"               "TAB q" (cmd! (bah/create|switch "queue"))
      )
