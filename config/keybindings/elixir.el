;;;; config/keybindings/elixir.el -*- lexical-binding: t; -*-

(defvar-local bah/current-overlay nil
  "Stores the currently displayed overlay.")

(defvar-local bah/elixir-eval-output nil
  "Stores captured output from Elixir evaluation.")

(defun bah/display-overlay (text &optional pos)
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
             (not (eq this-command 'bah/display-overlay)))
    (delete-overlay bah/current-overlay)
    (setq bah/current-overlay nil)
    (remove-hook 'post-command-hook #'bah/clear-overlay-on-move t)))

(defun bah/elixir-eval-and-display (form-fn)
  "Evaluate Elixir code using FORM-FN and display result in an overlay."
  (setq bah/elixir-eval-output nil)
  (let* ((proc (apprentice-iex-process))
         (orig-filter (process-filter proc))
         (buffer (current-buffer))
         (pos (point)))

    ;; Set up a temporary filter to capture output
    (set-process-filter
     proc
     (lambda (process output)
       ;; Capture the output
       (setq bah/elixir-eval-output
             (concat bah/elixir-eval-output output))

       ;; Extract the result (last line of output)
       (when (and (string-match "\n\\(.*\\)$" output)
                  (not (string-match "^iex" (match-string 1 output))))
         (with-current-buffer buffer
           (bah/display-overlay (string-trim (match-string 1 output)) pos)))

       ;; Pass the output to the original filter
       (funcall orig-filter process output)))

    ;; Execute the code evaluation function
    (funcall form-fn)

    ;; Restore the original filter after a delay
    (run-with-timer
     0.2 nil
     (lambda ()
       (when (process-live-p proc)
         (set-process-filter proc orig-filter))))))

;; Define interactive functions for different evaluation types
(defun bah/eval-last-sexp ()
  "Evaluate the last sexp and display the result in an overlay."
  (interactive)
  (bah/elixir-eval-and-display #'apprentice-iex-send-last-sexp))

(defun bah/eval-current-line ()
  "Evaluate the current line and display the result in an overlay."
  (interactive)
  (bah/elixir-eval-and-display #'apprentice-iex-send-current-line))

(defun bah/eval-defun ()
  "Evaluate the current defun and display the result in an overlay."
  (interactive)
  (bah/elixir-eval-and-display
   (lambda ()
     (save-excursion
       (mark-defun)
       (apprentice-iex-send-region (region-beginning) (region-end))))))

;; Set up keybindings
(map! :map elixir-mode-map
      :localleader
      :desc "Compile buffer"            "e b" #'apprentice-iex-compile-this-buffer
      :desc "Eval last sexp"            "e e" #'bah/eval-last-sexp
      :desc "Eval line"                 "e l" #'bah/eval-current-line
      :desc "Eval last defun"           "e d" #'bah/eval-defun
      :desc "Reload module"             "i R" #'apprentice-iex-reload-module
      :desc "Run 'iex -S mix'"          "i r" #'apprentice-iex-project-run
      :desc "Toggle file/tests"         "p t" #'apprentice-project-toggle-file-and-tests
      :desc "Run tests for file"        "p r" #'apprentice-project-run-tests-for-current-file)
