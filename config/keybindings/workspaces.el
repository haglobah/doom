;;; config/keybindings/workspaces.el -*- lexical-binding: t; -*-

(defvar bah/workspaces '(("doom" "~/.config/doom/" "config.el")
                         ("nix-home" "~/nix-home/" "home.nix")
                         ("mynix" "~/mynix/" "configuration.nix")
                         ("beathagenlocher.com" "~/beathagenlocher.com/" "flake.nix")
                         ("mycelium" "~/mycelium/" "Projects.md")
                         ))

(defun bah/setup-workspaces ()
  (interactive)

  (dolist (name+dir+file bah/workspaces)
    (persp-add-new (car name+dir+file))
    (persp-switch (car name+dir+file))
    (find-file (concat (cadr name+dir+file) (caddr name+dir+file)))))

(map! :leader
      :desc "Setup Workspaces"    "TAB w" #'bah/setup-workspaces
      :desc "Delete Workspace"    "TAB x" #'+workspace/kill
      :desc "New Workspace"       "TAB N" #'+workspace/new

      :desc "doom"                "TAB d" (cmd! (+workspace-switch "doom"))
      :desc "nix-home"            "TAB n" (cmd! (+workspace-switch "nix-home"))
      :desc "mynix"               "TAB m" (cmd! (+workspace-switch "mynix"))
      :desc "beathagenlocher.com" "TAB b" (cmd! (+workspace-switch "beathagenlocher.com"))
      :desc "mycelium"            "TAB o" (cmd! (+workspace-switch "mycelium"))
      )
