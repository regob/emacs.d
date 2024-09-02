;;; init-appearance.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq custom-safe-themes t)

(use-package gruvbox-theme
  :init
  (add-hook 'elpaca-after-init-hook #'(lambda () (load-theme 'gruvbox t)))
  )

;; Remove toolbars, menu and scrollbars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq visible-bell t                    ; Flash when bell would be emitted
      frame-inhibit-implied-resize t    ; Keep frame size
      inhibit-startup-message t         ; Hide the startup message
      )

(column-number-mode)
(global-hl-line-mode)

;; start in full screen
(push '(fullscreen . maximized) default-frame-alist)


;; if gui do something in whatver type of emacs instance we are using
;; adapted from https://www.reddit.com/r/emacs/comments/pc189c/comment/hafl5os/
(defun rb-apply-if-gui (&rest action)
  "Do specified ACTION if we're in a gui regardless of daemon or not."
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook
                #'(lambda ()
                    (if (display-graphic-p (selected-frame))
                        (apply action))
                    ))
    (if (display-graphic-p)
        (apply action))))

;; ----------------------------------------------------------------------------
;; Font config
;; ----------------------------------------------------------------------------

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

;; Initialize fonts if running with a GUI
(rb-apply-if-gui 'rb-init-font)



;; Enable line numbers only when executing goto-line
;; from http://whattheemacsd.com/
(defun rb-goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))
(global-set-key [remap goto-line] 'rb-goto-line-with-feedback)

(use-package anzu
  :config
  (setq anzu-mode-lighter "")
  (global-anzu-mode)
  )

(use-package breadcrumb
  :config
  (breadcrumb-mode)
  )

(provide 'init-appearance)

;;; init-appearance.el ends here

