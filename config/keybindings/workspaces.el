;;; config/keybindings/workspaces.el -*- lexical-binding: t; -*-


(defun bah/setup-workspaces ()
  (interactive)

  (dolist (name+dir '(("mycelium" "~/mycelium/")
                      ("doom" "~/.config/doom/")
                      ("nix-home" "~/nix-home/")
                      ))
    (persp-add-new (car name+dir))
    m
    m
    (persp-switch (car name+dir))
    (dired (cdr name+dir))))

(map! :leader "TAB w" #'bah/setup-workspaces)
