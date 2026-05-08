;;; config/keybindings/surround.el -*- lexical-binding: t; -*-

;; embrace.el extends evil-surround with multi-character pairs and custom
;; text objects. Doom already wires evil-embrace into evil-surround, so
;; pressing S<key> / ys<motion><key> / cs<old><new> / ds<key> will route
;; through embrace whenever <key> is in `evil-embrace-evil-surround-keys'
;; (a buffer-local list).
;;
;; APIs for extension:
;;   (embrace-add-pair KEY LEFT RIGHT)          ; fixed string pairs
;;   (embrace-add-pair-regexp KEY LEFT-RX       ; custom text objects,
;;                            RIGHT-RX          ; e.g. \\foo{...} forms
;;                            CHANGE-FN SAVE-FN HELP)
;; After adding a pair, also push its key onto
;; `evil-embrace-evil-surround-keys' (use `bah/embrace-register' below)
;; so evil-surround delegates to embrace.

(defun bah/embrace-register (key left right)
  "Register a buffer-local embrace pair and route KEY through evil-embrace."
  (require 'embrace)
  (require 'evil-embrace)
  (embrace-add-pair key left right)
  (cl-pushnew key evil-embrace-evil-surround-keys))

(defun bah/markdown-embrace-pairs ()
  (bah/embrace-register ?* "**" "**")
  (bah/embrace-register ?_ "__" "__")
  (bah/embrace-register ?~ "~~" "~~"))
(add-hook 'markdown-mode-hook #'bah/markdown-embrace-pairs)
(add-hook 'gfm-mode-hook #'bah/markdown-embrace-pairs)
