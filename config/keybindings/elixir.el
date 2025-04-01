;;; config/keybindings/elixir.el -*- lexical-binding: t; -*-

(defvar-local bah/current-overlay nil
  "Stores the currently displayed overlay.")

(defun display-overlay (text &optional beg end)
  "Display TEXT in a CIDER-like overlay at POS or point.
   The overlay will disappear when cursor moves."
  (interactive "sText to display: ")
  ;; Clear any existing overlay first
  (when bah/current-overlay
    (delete-overlay bah/current-overlay)
    (setq bah/current-overlay nil))

  ;; Create new overlay
  (let* ((beg (or beg (point)))
         (end (or end (point)))
         (ov (make-overlay beg end))
         (formatted-text (propertize
                          (format " => %s" text)
                          'face '(:foreground "#7cb8bb" :weight bold))))
    ;; Set overlay properties
    (overlay-put ov 'after-string formatted-text)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'bah/elixir t)

    ;; Store the overlay
    (setq bah/current-overlay ov)))

(defun bah/run-elixir (expr)
  (shell-command-to-string (format "elixir -e 'IO.inspect(%s)'" expr)))

(defun bah/elixir-eval-show-region (beg end)
  (interactive (list (point) (mark)))
  (if (not (and beg end))
      (error "elixir-eval-region: No region selected")
    (deactivate-mark)
    (display-overlay
      (string-trim
       (let* ((region (buffer-substring-no-properties beg end)))
        (bah/run-elixir region)))
      beg (+ end 1))))

(map! :map elixir-mode-map
      :localleader
      :desc "Remove overlay"            "r" (cmd! (remove-overlays (point-min) (point-max) 'bah/elixir t))

      :desc "Eval region"               "e r" #'bah/elixir-eval-show-region
      :desc "Eval last sexp"            "e e" (cmd! (bah/elixir-eval-show-region (save-excursion (backward-sexp) (point)) (point)))
      :desc "Eval defun"                "e d" (cmd! (save-excursion
                                                      (mark-defun)
                                                      (bah/elixir-eval-show-region (region-beginning) (region-end))))

      :desc "Reload module"             "i R" #'apprentice-iex-reload-module
      :desc "Run 'iex -S mix'"          "i r" #'apprentice-iex-project-run

      :desc "Toggle file/tests"         "p t" #'apprentice-project-toggle-file-and-tests
      :desc "Run tests for file"        "p r" #'apprentice-project-run-tests-for-current-file)
