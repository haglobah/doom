;;; config/keybindings/elixir.el -*- lexical-binding: t; -*-

(defvar-local bah/current-overlay nil
  "Stores the currently displayed overlay.")

(defun display-overlay (text &optional pos)
  "Display TEXT in a CIDER-like overlay at POS or point.
   The overlay will disappear when cursor moves."
  (interactive "sText to display: ")
  ;; Clear any existing overlay first
  (when bah/current-overlay
    (delete-overlay bah/current-overlay)
    (setq bah/current-overlay nil))

  ;; Create new overlay
  (let* ((pos (or pos (point)))
         (ov (make-overlay pos pos))
         (formatted-text (propertize
                          (format " => %s" text)
                          'face '(:foreground "#7cb8bb" :weight bold))))
    ;; Set overlay properties
    (overlay-put ov 'after-string formatted-text)
    (overlay-put ov 'priority 100)

    ;; Store the overlay
    (setq bah/current-overlay ov)

    ;; Set up the hook to clear the overlay when cursor moves
    (add-hook 'post-command-hook #'bah/clear-overlay-on-move nil t)))

(defun bah/clear-overlay-on-move ()
  "Remove the overlay when cursor moves."
  (when (and bah/current-overlay
             (not (eq this-command 'display-overlay)))
    (delete-overlay bah/current-overlay)
    (setq bah/current-overlay nil)
    (remove-hook 'post-command-hook #'bah/clear-overlay-on-move t)))

(map! :map elixir-mode-map
      :localleader
      :desc "Compile buffer"            "e b" #'apprentice-iex-compile-this-buffer
      :desc "Eval last sexp"            "e e" (cmd! (display-overlay (apprentice-iex-send-last-sexp)))
      :desc "Eval line"                 "e l" (cmd! (display-overlay (apprentice-iex-send-current-line)))
      :desc "Eval last defun"
      "e d" (cmd!
             (display-overlay
              (save-excursion
                (mark-defun)
                (apprentice-iex-send-region (region-beginning) (region-end)))))

      :desc "Reload modlue"             "i R" #'apprentice-iex-reload-module
      :desc "Run 'iex -S mix'"          "i r" #'apprentice-iex-project-run

      :desc "Toggle file/tests"         "p t" #'apprentice-project-toggle-file-and-tests
      :desc "Run tests for file"        "p r" #'apprentice-project-run-tests-for-current-file)
