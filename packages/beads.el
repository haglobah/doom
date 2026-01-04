;;; packages/beads.el -*- lexical-binding: t; -*-

(use-package! beads)

(map! :leader
      :desc "Open beads" :nv "e b" #'beads-project)
