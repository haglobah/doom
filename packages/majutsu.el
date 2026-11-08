;;; packages/majutsu.el -*- lexical-binding: t; -*-

;; Magit-style jj frontend. Open it next to magit: SPC g g for git, SPC g j for jj.
(use-package! majutsu
  :commands (majutsu majutsu-log))

(map! :leader
      :desc "Majutsu (jj log)" :nv "g j" #'majutsu)
