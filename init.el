;;; init.el --- Emacs init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ====================================
;; Initialize package manager and paths
;; ====================================

;; Pre-config local settings, for setting paths, etc needed for setup.
(require 'init-local-pre nil t)

;; always load .el if newer than .elc
(setq load-prefer-newer t)

;; Example Elpaca configuration -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Should enable no-symlink mode on Windows
(if (eq system-type 'windows-nt)
    (elpaca-no-symlink-mode))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq elpaca-use-package-by-default t)
  )

;; ====================================
;; General setup
;; ====================================

;; (use-package diminish)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; save customizations to custom.el, if exists (which is git-ignored) instead of init.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file t)))

;; Set autosave directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq backup-by-copying t)

;; always ask before killing emacs (does not hold for emacsclient though)
(setq confirm-kill-emacs 'yes-or-no-p)

;; Set cat as pager, as less does not work well in eshell
(setenv "PAGER" "cat")

;; turn off auto fill 
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

;; =========================
;; User functions
;; =========================

(defun rb-jump-to-init-file ()
  "Open init.el in the current window."
  (interactive)
  (find-file user-init-file)
  )

(defun rb-byte-recompile ()
  "Byte recompile all init files."
  (interactive)
  (byte-recompile-file user-init-file 0)
  (byte-recompile-directory (expand-file-name "lisp" user-emacs-directory) 0)
  )

;; my user keymap
(define-prefix-command 'rb-user-keymap)
(global-set-key (kbd "C-c 8") 'rb-user-keymap)
(bind-key (kbd "i") 'rb-jump-to-init-file 'rb-user-keymap)
(bind-key (kbd "c") #'rb-byte-recompile 'rb-user-keymap)


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
  :config
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  )

;; =========================
;; Global development setup
;; =========================

;; enable flycheck globally
(use-package flycheck
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(use-package projectile
  :config
  (add-hook 'prog-mode-hook #'(lambda () (projectile-mode +1)))
  :bind-keymap
  ("C-c p" . 'projectile-command-map)
  :custom
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  (add-to-list 'projectile-globally-ignored-directories ".direnv")
  )

(use-package rainbow-delimiters
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

(use-package yasnippet
  :config
  ;; (add-to-list 'load-path
  ;;              "~/.emacs.d/plugins/yasnippet-radical-snippets")
  ;; (require 'yasnippet-radical-snippets)
  ;; (yasnippet-radical-snippets-initialize)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets)

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package envrc
  :config
  (envrc-global-mode)
  )


(use-package ess)
(use-package markdown-mode)

(use-package csv-mode)

(use-package flycheck-kotlin)

(use-package kotlin-mode
  :after flycheck
  )

;; (use-package treesit-auto
;;   :custom
;;   (treesit-auto-install 'prompt)
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

(require 'init-tex nil t)
(require 'init-sh nil t)
(require 'init-python nil t)
(require 'init-lisp nil t)
(require 'init-cc nil t)
(require 'init-web nil t)
(require 'init-org nil t)

(require 'init-editing-utils nil t)
(require 'init-consult nil t)
(require 'init-company nil t)
(require 'init-vc nil t)
(require 'init-appearance nil t)
(require 'init-windows nil t)
(require 'init-sessions nil t)
(require 'init-minibuffer nil t)

;; =========================
;; Trailer
;; =========================

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)


;;; init.el ends here
