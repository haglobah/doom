;;; config/keybindings/workspaces.el -*- lexical-binding: t; -*-


(defun bah/setup-workspaces ()
  (interactive)

  (progn (projectile-switch-project-by-name "~/ag/fabresearcher/work/")
         (kmacro "<return> f l a k e . n i x <return>")))

(map! :leader "TAB w" #'bah/setup-workspaces)
