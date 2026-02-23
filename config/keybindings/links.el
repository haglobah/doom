;;; config/keybindings/links.el -*- lexical-binding: t; -*-

(defvar bah/priv-links
  '("focus.nirvanahq.com"
    "eu.posthog.com"
    "https://linklonk.com/foryou/stats"
    "https://monitoring.hagenlocher.me/grafana/d/1e8e5fbf-7fc4-436c-85b8-245aa3f18f52/infrastructure-health?orgId=1&from=now-24h&to=now&timezone=browser&var-hostname=$__all&refresh=5s"))

(defvar bah/work-links
  `("mattermost.active-group.de"
    "timetracking.active-group.de"
    "gitlab.active-group.de/ag/"
    "projects.active-group.de/projects/fab-researcher/issues"
    "app.element.io"
    "plausible.io"
    ,@bah/priv-links))

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
