;;; racket.el --- racket                             -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Beat Hagenlocher

;; Author: Beat Hagenlocher <beat@gondor>

;; The feature is `racket-mode', not `racket' — `after! racket' never fires.
(after! racket-mode
  (setq racket-smart-open-bracket-mode nil))
