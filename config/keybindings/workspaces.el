;;; config/keybindings/workspaces.el -*- lexical-binding: t; -*-

(defvar bah/workspaces '(("doom" "~/.config/doom/" "config.el")
                         ("nix-home" "~/nix-home/" "home.nix")
                         ("mynix" "~/mynix/" "configuration.nix")
                         ("beathagenlocher.com" "~/beathagenlocher.com/" "flake.nix")
                         ("mycelium" "~/mycelium/" "acc.md")
                         ("templater" "~/projects/templater/" "templates/flake.nix")

                         ("fabresearcher" "~/ag/fabresearcher/work/" "Justfile")
                         ("queue" "~/projects/rescript_instant_todo/" "flake.nix")
                         ))

(defun bah/setup-workspaces (workspaces)
  (interactive)

  (dolist (name+dir+file workspaces)
    (persp-add-new (first name+dir+file))
    (persp-switch (first name+dir+file))
    (find-file (concat (second name+dir+file) (caddr name+dir+file)))
    (find-file (concat (second name+dir+file) (second (projectile-recentf-files))))))

(map! :leader
      :desc "Setup Workspaces"    "TAB w" (cmd! (bah/setup-workspaces bah/workspaces))
      :desc "Delete Workspace"    "TAB x" #'+workspace/kill
      :desc "New Workspace"       "TAB N" #'+workspace/new

      :desc "Launch kitty tab"    "TAB ," (cmd! (shell-command (concat "kitty @ --to=unix:/tmp/mykitty launch --type=tab --cwd=" (projectile-project-root (file-name-directory (buffer-file-name))) " fish")))

      :desc "doom"                "TAB d" (cmd! (+workspace-switch "doom"))
      :desc "nix-home"            "TAB n" (cmd! (+workspace-switch "nix-home"))
      :desc "mynix"               "TAB m" (cmd! (+workspace-switch "mynix"))
      :desc "beathagenlocher.com" "TAB b" (cmd! (+workspace-switch "beathagenlocher.com"))
      :desc "mycelium"            "TAB o" (cmd! (+workspace-switch "mycelium"))
      :desc "templater"           "TAB t" (cmd! (+workspace-switch "templater"))

      :desc "fabresearcher"       "TAB f" (cmd! (+workspace-switch "fabresearcher"))
      :desc "queue"               "TAB q" (cmd! (+workspace-switch "queue"))
      )
