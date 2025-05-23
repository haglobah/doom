;;; packages/gptel.el -*- lexical-binding: t; -*-

;; Can you add sensible keybindings to this package?
(defun get-apikey-from-env ()
  (getenv "OPENAI_API_KEY"))

(use-package! gptel

  :init
  (map! :leader
        :desc "Open gptel buffer" :nv "o g" #'gptel)
 
  :config
  (setq! gptel-api-key #'get-apikey-from-env)
  (setq! gptel-model "o4-mini"))

(map! :localleader
      :desc "gptel: Send region" :nv "a s" #'gptel-send
      ;; :desc "gptel: Send region (with options)" :nv "a u" (cmd! (universal-argument) (gptel-send))
      :desc "gptel: Rewrite region" :nv "a r" #'gptel-rewrite
      :desc "gptel: Add region/buffer to context" :nv "a a" #'gptel-add
      :desc "gptel: Add file to context" :nv "a f" #'gptel-add-file
      :desc "gptel: Menu" :nv "a m" #'gptel-menu
      :desc "gptel: Abort" :nv "a b" #'gptel-abort)

;; Below is an updated version with improved keybindings. Note that the duplicate "a m" keybinding (for both “Menu” and “Abort”) has been fixed by remapping “Abort” to “a b”:

;; ------------------------------------------------------------
;; ;;; packages/gptel.el -*- lexical-binding: t; -*-

;; ;; Function to retrieve the API key from the environment.
;; (defun get-apikey-from-env ()
;;   (getenv "OPENAI_API_KEY"))

;; (use-package! gptel
;;   :init
;;   (map! :leader
;;         :desc "Open gptel buffer" :nv "o g" #'gptel)
;;   :config
;;   (setq! gptel-api-key #'get-apikey-from-env))

;; (map! :localleader
;;       :desc "gptel: Send region"                :nv "a s" #'gptel-send
;;       :desc "gptel: Send region (with options)"   :nv "a u" (cmd! (universal-argument) (gptel-send))
;;       :desc "gptel: Rewrite region"              :nv "a r" #'gptel-rewrite
;;       :desc "gptel: Add region/buffer to context"  :nv "a a" #'gptel-add
;;       :desc "gptel: Add file to context"           :nv "a f" #'gptel-add-file
;;       :desc "gptel: Menu"                          :nv "a m" #'gptel-menu
;;       :desc "gptel: Abort"                         :nv "a b" #'gptel-abort)
;; ------------------------------------------------------------

;; This setup binds global keys under your leader key (“o g”) to open the gptel buffer, and local leader keys for region-based commands (under “a” scope) with clear descriptions.)
