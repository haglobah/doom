;;; config/keybindings/workspaces.el -*- lexical-binding: t; -*-

(defun bah/setup-workspaces ()
  (interactive)

  (dolist (name+dir+file '(("mycelium" "~/mycelium/" "Projects.md")
                           ("doom" "~/.config/doom/" "config.el")
                           ("nix-home" "~/nix-home/" "home.nix")
                           ))
    (persp-add-new (car name+dir+file))
    (persp-switch (car name+dir+file))
    (find-file (concat (cadr name+dir+file) (caddr name+dir+file)))))

(map! :leader "TAB w" #'bah/setup-workspaces)
