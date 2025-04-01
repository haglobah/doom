;;; config/keybindings/elixir.el -*- lexical-binding: t; -*-

(defvar-local bah/current-overlay nil
  "Stores the currently displayed overlay.")

(defvar bah/iex-process nil
  "Stores the current IEx process.")

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

(defun bah/ensure-iex-process ()
  "Ensure that an IEx process is running and return it."
  (unless (and bah/iex-process
               (process-live-p bah/iex-process))
    (let ((buffer (get-buffer-create "*iex*")))
      (with-current-buffer buffer
        (setq bah/iex-process
              (start-process "iex" buffer "iex")))
      (set-process-filter bah/iex-process #'bah/iex-process-filter)
      ;; Wait a bit for IEx to start
      (sleep-for 0.5)))
  bah/iex-process)

(defvar bah/iex-output ""
  "Temporary storage for IEx output.")

(defvar bah/iex-waiting nil
  "Flag to indicate we're waiting for IEx output.")

(defvar bah/iex-current-overlay-region nil
  "Region where the overlay should be displayed.")

(defun bah/iex-process-filter (process output)
  "Process filter for the IEx process."
  (setq bah/iex-output (concat bah/iex-output output))

  ;; Check if output is complete (contains a prompt)
  (when (and bah/iex-waiting
             (string-match "iex([0-9]+)>" bah/iex-output))
    (let ((result (string-trim
                   (replace-regexp-in-string
                    "iex([0-9]+)>" ""
                    (replace-regexp-in-string
                     "\r" ""
                     (ansi-color-filter-apply bah/iex-output))))))

      ;; Remove trailing "nil" on its own line
      (setq result (replace-regexp-in-string "\n\\s-*nil\\s-*$" "" result))
      ;; Remove leading newlines
      (setq result (string-trim result))
      ;; Display the result as an overlay
      (when bah/iex-current-overlay-region
        (let ((beg (car bah/iex-current-overlay-region))
              (end (cdr bah/iex-current-overlay-region)))
          (display-overlay result beg end)))

      ;; Reset state
      (setq bah/iex-waiting nil
            bah/iex-output ""
            bah/iex-current-overlay-region nil))))

(defun bah/iex-eval (expr beg end)
  "Evaluate EXPR in IEx and show results as an overlay at BEG to END."
  (bah/ensure-iex-process)

  ;; Set up for waiting on response
  (setq bah/iex-waiting t
        bah/iex-output ""
        bah/iex-current-overlay-region (cons beg end))

  ;; Send expression to IEx
  (process-send-string bah/iex-process (concat expr "\n")))

(defun bah/elixir-eval-show-region (beg end)
  (interactive (list (mark) (point)))
  (if (not (and beg end))
      (error "elixir-eval-region: No region selected")
    (deactivate-mark)
    (let ((region (buffer-substring-no-properties beg end)))
      ;; (message region)
      (bah/iex-eval region beg end))))

(defun bah/restart-iex ()
  "Restart the IEx process."
  (interactive)
  (when (and bah/iex-process (process-live-p bah/iex-process))
    (delete-process bah/iex-process))
  (setq bah/iex-process nil)
  (bah/ensure-iex-process)
  (message "IEx process restarted"))

(defun bah/iex-project-run ()
  "Run IEx with the current Mix project."
  (interactive)
  (when (and bah/iex-process (process-live-p bah/iex-process))
    (delete-process bah/iex-process))
  (setq bah/iex-process nil)

  (let ((buffer (get-buffer-create "*iex-mix*")))
    (with-current-buffer buffer
      (erase-buffer)
      (setq bah/iex-process
            (start-process "iex-mix" buffer "iex" "-S" "mix")))
    (set-process-filter bah/iex-process #'bah/iex-process-filter)
    (display-buffer buffer))
  (message "IEx started with Mix project"))

(map! :map elixir-mode-map
      :localleader
      :desc "Remove overlay"            "r" (cmd! (remove-overlays (point-min) (point-max) 'bah/elixir t))
      :desc "Eval region"               "e r" #'bah/elixir-eval-show-region
      :desc "Eval last sexp"            "e e" (cmd! (save-excursion (mark-sexp) (bah/elixir-eval-show-region (region-beginning) (region-end))))
      :desc "Eval defun"                "e d" (cmd! (save-excursion
                                                      (mark-defun)
                                                      (bah/elixir-eval-show-region (region-beginning) (region-end))))
      :desc "Eval buffer"               "e b" (cmd! (save-excursion
                                                      (mark-whole-buffer)
                                                      (bah/elixir-eval-show-region (region-beginning) (region-end))))
      :desc "Restart IEx"               "i r" #'bah/restart-iex
      :desc "Run 'iex -S mix'"          "i m" #'bah/iex-project-run
      :desc "Toggle file/tests"         "p t" #'apprentice-project-toggle-file-and-tests
      :desc "Run tests for file"        "p r" #'apprentice-project-run-tests-for-current-file)
