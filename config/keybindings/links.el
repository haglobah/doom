;;; config/keybindings/links.el -*- lexical-binding: t; -*-

(defvar bah/standard-links
  '("mattermost.active-group.de"
    "timetracking.active-group.de"
    ;; "projects.active-group.de"
    "focus.nirvanahq.com"))

(map! :leader
      :prefix ("e" . "bah")
      :desc "Open: Work links" :nv "w"
      (cmd! (shell-command
             (concat "kitty @ --to=unix:@mykitty launch --type=tab firefox "
                     (mapconcat
                      (lambda (link) (concat "--new-tab " link))
                      bah/standard-links
                      " ")))))
