;;; init.el --- Emacs init file  -*- lexical-binding: t; -*-


;; ====================================
;; Initialize package manager and paths
;; ====================================

;; Pre-config local settings, for setting paths, etc needed for setup.
(require 'init-local-pre nil t)


(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; use-package is not needed at runtime
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(add-to-list 'load-path (expand-file-name "plugins" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; save customizations to custom.el (which is ignored) instead of init.el
(setq custom-file (locate-user-emacs-file "custom.el"))

;; Recompile the init files
(byte-recompile-file user-init-file)
(byte-recompile-directory (expand-file-name "lisp" user-emacs-directory) 0)



;; Set autosave directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq backup-by-copying t)

;;(setq initial-frame-alist '((top . 0) (left . 0) (width . 120) (height . 80)))

(global-set-key (kbd "C-x 4 s") 'forward-symbol)

;; always ask before killing emacs (does not hold for emacsclient though)
(setq confirm-kill-emacs 'yes-or-no-p)



;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))
;; to update packages: list-packages then S-u x



;; Enable line numbers only when executing goto-line
;; from http://whattheemacsd.com/
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))


(setenv "PAGER" "cat")
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package centered-cursor-mode
  :init
  ;; (global-centered-cursor-mode)
  ;; (add-hook 'prog-mode-hook #'centered-cursor-mode)
  ;; (add-hook 'text-mode-hook #'centered-cursor-mode)
  )


;; (use-package treemacs)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

;; =========================
;; User functions
;; =========================

(defun rb-jump-to-init-file ()
  (interactive)
  (find-file user-init-file)
  ;; (switch-to-buffer "~/.emacs.d/init.el")
  )

;; my user keymap
(define-prefix-command 'rb-user-keymap)
(global-set-key (kbd "C-c 8") 'rb-user-keymap)
(bind-key (kbd "i") 'rb-jump-to-init-file 'rb-user-keymap)


(global-set-key (kbd "C-x C-b") #'ibuffer)

;; macro keymap
(define-prefix-command 'rb-macro-keymap)
(global-set-key (kbd "C-c m") 'rb-macro-keymap)
(bind-key (kbd "s") #'start-kbd-macro 'rb-macro-keymap)
(bind-key (kbd "e") #'end-kbd-macro 'rb-macro-keymap)
(bind-key (kbd "q") #'kbd-macro-query 'rb-macro-keymap)
(bind-key (kbd "r") #'call-last-kbd-macro 'rb-macro-keymap)
(bind-key (kbd "m") #'edit-last-kbd-macro 'rb-macro-keymap)

(use-package avy
  :init
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  )

;; =========================
;; Global development setup
;; =========================

;; enable flycheck globally
(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(use-package projectile
  :init
  (add-hook 'prog-mode-hook #'(lambda () (projectile-mode +1)))
  :bind-keymap
  ("C-c p" . 'projectile-command-map)
  :custom
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  (add-to-list 'projectile-globally-ignored-directories ".direnv")
  )

(use-package rainbow-delimiters
  :init
  ;; turn on rainbow delims in all programming languages and LaTeX
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)

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
  )

(use-package aggressive-indent
  :hook
  ((css-mode . aggressive-indent-mode))
  )

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  ;; (add-to-list 'load-path
  ;;              "~/.emacs.d/plugins/yasnippet-radical-snippets")
  ;; (require 'yasnippet-radical-snippets)
  ;; (yasnippet-radical-snippets-initialize)
  )

(use-package yasnippet-snippets)

(electric-pair-mode -1)
(electric-indent-mode -1) ; TODO: still gets turned on in cc-mode









(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package envrc
  :init
  (add-hook 'after-init-hook 'envrc-global-mode)
  )


(use-package ess)
(use-package markdown-mode)

(use-package csv-mode)

(use-package flycheck-kotlin)

(use-package kotlin-mode
  :init
  :after flycheck
  :config
  )

(require 'init-editing-utils nil t)
(require 'init-company nil t)
(require 'init-vc nil t)
(require 'init-appearance nil t)
(require 'init-windows nil t)
(require 'init-sessions nil t)
(require 'init-minibuffer nil t)
(require 'init-lisp nil t)
(require 'init-cc nil t)
(require 'init-web nil t)
(require 'init-org nil t)
(require 'init-tex nil t)
(require 'init-sh nil t)
(require 'init-python nil t)

;; =========================
;; Trailer
;; =========================

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

(put 'dired-find-alternate-file 'disabled nil)

;;; init.el ends here
