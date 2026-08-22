;;; config/keybindings/elixir.el -*- lexical-binding: t; -*-

;; apprentice (maintained alchemist fork) provides the IEx process handling
;; that used to be hand-rolled here. Results appear in the IEx comint buffer;
;; the fork dropped alchemist's eval server, so there are no inline overlays.

;; Upstream only autoloads the two `*-run' commands; the rest need explicit
;; autoloads (apprentice.el transitively requires the iex/project sub-files).
(use-package! apprentice
  :commands (apprentice-iex-send-region
             apprentice-iex-send-region-and-go
             apprentice-iex-send-last-sexp
             apprentice-iex-send-current-line
             apprentice-iex-compile-this-buffer
             apprentice-project-toggle-file-and-tests
             apprentice-project-run-tests-for-current-file))

(map! :map (elixir-mode-map elixir-ts-mode-map)
      :localleader
      :desc "Send region to IEx"        "e r" #'apprentice-iex-send-region
      :desc "Send region to IEx and go" "e R" #'apprentice-iex-send-region-and-go
      :desc "Send last sexp to IEx"     "e e" #'apprentice-iex-send-last-sexp
      :desc "Send line to IEx"          "e l" #'apprentice-iex-send-current-line
      :desc "Compile buffer in IEx"     "e b" #'apprentice-iex-compile-this-buffer
      :desc "Run IEx"                   "i i" #'apprentice-iex-run
      :desc "Run 'iex -S mix'"          "i m" #'apprentice-iex-project-run
      :desc "Toggle file/tests"         "p t" #'apprentice-project-toggle-file-and-tests
      :desc "Run tests for file"        "p r" #'apprentice-project-run-tests-for-current-file)
