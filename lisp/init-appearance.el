;;; init-appearance.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(use-package gruvbox-theme
  :ensure t
  :init
  (add-hook 'after-init-hook (lambda() (load-theme 'gruvbox t)))
  )


;; Remove toolbars, menu and scrollbars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)    ; Hide the startup message

(defun rb-init-font ()
  "Initialize display font from a list of options."
  
  ;; Select font installed in the order of priority
  ;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
  (cond
   ((member "Source Code Pro" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Source Code Pro"
                        :height 95))
   ((member "Consolas" (font-family-list))
    (set-face-attribute 'default nil
                        :family "Consolas"
                        :height 100))
   ((member "DejaVu Sans Mono" (font-family-list))
    (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :height 100))
   (t
    (warn "Default fonts are not installed. Make sure to install one or select another one!"))
   )

  (copy-face 'default 'fixed-pitch)
  )

(add-hook 'after-init-hook #'rb-init-font)


(use-package anzu
  :init
  (add-hook 'after-init-hook 'global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  )

(use-package minions
  :ensure t
  :init
  (setq minions-mode-line-lighter "☰")
  (minions-mode 1))


(provide 'init-appearance)

;;; init-appearance.el ends here

