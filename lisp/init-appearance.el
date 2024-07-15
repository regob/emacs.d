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


;; Select font installed in the order of priority
;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
(cond
 ((member "Source Code Pro" (font-family-list))
  (set-face-attribute 'default nil
                      :family "Source Code Pro"
                      :height 100))
 ;; on Windows its written without spaces
 ((member "SourceCodePro" (font-family-list))
  (set-face-attribute 'default nil
                      :family "SourceCodePro"
                      :height 100))
 (t
  (warn "Default fonts are not installed. Make sure to install one or select another one!"))
 )



(use-package minions
  :ensure t
  :init
  (setq minions-mode-line-lighter "☰")
  (minions-mode 1))


(provide 'init-appearance)

;;; init-appearance.el ends here

