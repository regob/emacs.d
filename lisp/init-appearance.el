;;; init-appearance.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(use-package gruvbox-theme
  :ensure t ;; (gruvbox-theme :files ("*.el"))
  :init
  (add-hook 'elpaca-after-init-hook #'(lambda () (load-theme 'gruvbox t)))
  )

;; (use-package uwu-theme
;;   :ensure (uwu-theme :host github :repo "kborling/uwu-theme" :files ("*.el"))
;;   :config
;;   (setq
;;    uwu-distinct-line-numbers 'nil
;;    uwu-scale-org-headlines t
;;    uwu-use-variable-pitch t)
;;   (load-theme 'uwu t))

;; Remove toolbars, menu and scrollbars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; start in full screen
(push '(fullscreen . maximized) default-frame-alist)

(setq inhibit-startup-message t)    ; Hide the startup message

(defun rb-init-font ()
  "Initialize display font from a list of options."
  (interactive)
  
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

(add-hook 'window-setup-hook #'rb-init-font)


;; Enable line numbers only when executing goto-line
;; from http://whattheemacsd.com/
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; (use-package anzu
;;   :config
;;   (setq anzu-mode-lighter "")
;;   (global-anzu-mode)
;;   )

;; (use-package minions
;;   :config
;;   (setq minions-mode-line-lighter "☰")
;;   (minions-mode 1)
;;   )


(provide 'init-appearance)

;;; init-appearance.el ends here

