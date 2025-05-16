;;; config/keybindings/workspaces.el -*- lexical-binding: t; -*-

(defvar bah/workspaces '(("doom" "~/.config/doom/" "config.el")
                         ("nix-home" "~/nix-home/" "home.nix")
                         ("mynix" "~/mynix/" "configuration.nix")
                         ("beathagenlocher.com" "~/beathagenlocher.com/" "flake.nix")
                         ("mycelium" "~/mycelium/" "acc.md")

                         ("fabresearcher" "~/ag/fabresearcher/work/" "Justfile")
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

      :desc "doom"                "TAB d" (cmd! (+workspace-switch "doom"))
      :desc "nix-home"            "TAB n" (cmd! (+workspace-switch "nix-home"))
      :desc "mynix"               "TAB m" (cmd! (+workspace-switch "mynix"))
      :desc "beathagenlocher.com" "TAB b" (cmd! (+workspace-switch "beathagenlocher.com"))
      :desc "mycelium"            "TAB o" (cmd! (+workspace-switch "mycelium"))

      :desc "fabresearcher"       "TAB f" (cmd! (+workspace-switch "fabresearcher"))
      )
