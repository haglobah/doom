;;; -*- lexical-binding: t; -*-

(after! avy
  (setq! avy-timeout-seconds 0.1)
  (map! :nv "DEL ," #'avy-goto-char-timer))
