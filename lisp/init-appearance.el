;;; init-appearance.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ----------------------------------------------------------------------------
;; Theme setup
;; ----------------------------------------------------------------------------

(setq custom-safe-themes t)

(use-package gruvbox-theme
  :ensure (:files ("*.el") :autoloads nil)
  :init
  (add-hook 'elpaca-after-init-hook #'(lambda () (load-theme 'gruvbox t)))
  :config
  ;; Modify some faces I don't like in the gruvbox theme
  (custom-set-faces
   ;; matches are too bright by default (gray on blue)
   '(match ((t (:background "#554444" :foreground "#999999")))))
  )


;; ----------------------------------------------------------------------------
;; Improve some visuals
;; ----------------------------------------------------------------------------


;; Remove toolbars, menu and scrollbars
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq visible-bell t                    ; Flash when bell would be emitted
      inhibit-startup-message t         ; Hide the startup message
      )

(column-number-mode)
(global-hl-line-mode)


;; if gui do something in whatever type of emacs instance we are using
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
;; Frame specific setup (only in graphical environments)
;; ----------------------------------------------------------------------------

(defun rb-frame-settings ()
  (scroll-bar-mode -1)
  ;; start in full screen
  (push '(fullscreen . maximized) default-frame-alist)
  (setq frame-inhibit-implied-resize t)    ; Keep frame size
  )

(rb-apply-if-gui 'rb-frame-settings)

;; ----------------------------------------------------------------------------
;; Font config
;; ----------------------------------------------------------------------------


(defun rb--init-general-font (font-name &rest args)
  "Initialize FONT-NAME for many general faces.
Also pass ARGS to `set-face-attribute' calls."
  (dolist (face '(default dictionary-word-definition-face))
    (apply 'set-face-attribute
           face
           nil
           :family font-name
           args
           )))

(defun rb/init-font ()
  "Initialize display font from a list of options."
  (interactive)
  
  ;; Select font installed in the order of priority
  ;; https://emacsredux.com/blog/2021/12/22/check-if-a-font-is-available-with-emacs-lisp/
  (cond
   ((member "Source Code Pro" (font-family-list))
    (rb--init-general-font "Source Code Pro"
                           :height 95))
   ((member "Consolas" (font-family-list))
    (rb--init-general-font "Consolas"
                           :height 100))
   ((member "DejaVu Sans Mono" (font-family-list))
    (rb--init-general-font "DejaVu Sans Mono"
                           :height 100))
   (t
    (warn "Default fonts are not installed. Make sure to install one or select another one!"))
   )

  (copy-face 'default 'fixed-pitch)
  )

;; Initialize fonts if running with a GUI
(add-hook 'elpaca-after-init-hook #'(lambda () (rb-apply-if-gui 'rb/init-font)))

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

;; ----------------------------------------------------------------------------
;; Small utilities for changing modeline/fringe/etc
;; ----------------------------------------------------------------------------


(use-package anzu
  :ensure (:host github :repo "emacsorphanage/anzu")
  :config
  (setq anzu-mode-lighter "")
  (global-anzu-mode)
  )

(use-package breadcrumb
  :ensure (:source "ELPA")
  :config
  (breadcrumb-mode)
  )

;; ----------------------------------------------------------------------------
;; Indentation guides 
;; ----------------------------------------------------------------------------

(use-package highlight-indent-guides
  :ensure (:source "MELPA")
  :diminish
  :hook
  ((prog-mode . highlight-indent-guides-mode)
   (yaml-mode . highlight-indent-guides-mode)
   (js-json-mode . highlight-indent-guides-mode))
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-enabled nil)
  :config
  (set-face-foreground 'highlight-indent-guides-character-face "#344")
  (set-face-foreground 'highlight-indent-guides-top-character-face "#aaa")
  )

;; ----------------------------------------------------------------------------
;; Rainbow delims
;; ----------------------------------------------------------------------------

(use-package rainbow-delimiters
  :ensure (:source "MELPA")
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66") ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6") ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f") ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6") ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc") ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c") ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc") ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999") ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666") ; dark gray
  ;; turn on rainbow delims in all programming languages and LaTeX
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook 'rainbow-delimiters-mode)
  )

(provide 'init-appearance)

;;; init-appearance.el ends here

