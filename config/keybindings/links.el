;;; config/keybindings/links.el -*- lexical-binding: t; -*-

(defvar bah/priv-links
  '("focus.nirvanahq.com"
    "app.mochi.cards"))

(defvar bah/work-links
  '("mattermost.active-group.de"
    "timetracking.active-group.de"
    "focus.nirvanahq.com"
    "app.mochi.cards"
    "https://projects.active-group.de/projects/fab-researcher/issues"))

(defun bah/open-in-tabs (linklist)
  (let* ((command (bah/i (concat "kitty @ --to=unix:@mykitty launch --type=tab firefox "
                                 (mapconcat
                                  (lambda (link) (concat "--new-tab \"" link))
                                  linklist
                                  "\" ")
                                 "\""))))
    command
    (shell-command command)
    ))

(map! :leader
      :prefix ("e" . "bah")
      :desc "Open: Work links" :nv "w" (cmd! (bah/open-in-tabs bah/work-links))
      :desc "Open: Private links" :nv "p" (cmd! (bah/open-in-tabs bah/priv-links))
      )
