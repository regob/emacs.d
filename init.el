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
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
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

(add-hook 'elpaca-after-init-hook #'(lambda () (message "Elpaca after init hook running ...")))
(add-hook 'elpaca-post-queue-hook #'(lambda () (message "Elpaca post queue hook running ...")))
(advice-add 'elpaca-after-init-hook :after #'(lambda (&rest r) (message "Elpaca after-init-hook finished!")) )

;; ====================================
;; Packages required for use-package
;; ====================================

(use-package diminish
  :ensure (:source "MELPA"))

(elpaca-wait)

;; ----------------------------------------------------------------------------
;; Update some builtin packages
;; ----------------------------------------------------------------------------

(use-package transient
  :ensure (:source "MELPA")
  )

;; Magit requires seq>=2.24, but it's built-in, elpaca rule to be added
;; (use-package seq
;;   :ensure (:source "GNU-devel ELPA")
;;   )

;; ====================================
;; General setup
;; ====================================

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; save customizations to custom.el, if exists (which is git-ignored) instead of init.el
(setq custom-file (locate-user-emacs-file "custom.el"))
(add-hook 'elpaca-after-init-hook #'(lambda () (load custom-file t)))

;; Set autosave directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq backup-by-copying t)

;; always ask before killing emacs (does not hold for emacsclient though)
(setq confirm-kill-emacs 'yes-or-no-p)

;; some performance settings (partly for lsp-mode)
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; remove scratch message
(setq initial-scratch-message "")

;; ----------------------------------------------------------------------------
;; Some "global" keymaps
;; ----------------------------------------------------------------------------

(define-prefix-command 'rb-lispy-keymap)
(global-set-key (kbd "C-c e") 'rb-lispy-keymap)
(bind-key (kbd "r") 'eval-region 'rb-lispy-keymap)
(bind-key (kbd "b") 'eval-buffer 'rb-lispy-keymap)
(bind-key (kbd "t") 'transpose-sexps 'rb-lispy-keymap)

(define-prefix-command 'rb-user-keymap)
(global-set-key (kbd "C-c 8") 'rb-user-keymap)

;; ----------------------------------------------------------------------------
;; Initialize all packages
;; ----------------------------------------------------------------------------


(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

(use-package so-long
  :ensure nil
  :config
  (global-so-long-mode))

(use-package ess)

(require 'init-utils nil nil)
(require 'init-appearance nil nil)
(require 'init-windows nil nil)

(require 'init-minibuffer nil nil)
(require 'init-dired nil nil)
(require 'init-ibuffer nil nil)
(require 'init-help nil nil)
(require 'init-consult nil nil)
(require 'init-vc nil nil)
(require 'init-sessions nil nil)
(require 'init-project nil nil)

;; General editor functionality
(require 'init-eglot nil nil)
(require 'init-completion nil nil)
(require 'init-editing-utils nil nil)
(require 'init-smartparens nil nil)
(require 'init-format-all nil nil)
(require 'init-flymake nil nil)

(elpaca-wait)

;; language modes
(require 'init-tex nil nil)
(require 'init-sh nil nil)
(require 'init-python nil nil)
(require 'init-kotlin nil nil)
(require 'init-lisp nil nil)
(require 'init-cc nil nil)
(require 'init-web nil nil)
(require 'init-org nil nil)
(require 'init-markup nil nil)
(require 'init-powershell nil nil)
(require 'init-shell nil nil)

(elpaca-wait)

;; ----------------------------------------------------------------------------
;; Load private libraries
;; ----------------------------------------------------------------------------

;; adapted from https://stackoverflow.com/a/21767679/11579038
(defun rb/load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let* ((libraries-loaded (mapcar #'file-name-sans-extension
                                   (delq nil (mapcar #'car load-history))))
         (num-libs (length libraries-loaded)))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))
    (message "%d private library file(s) loaded from %s"
             (- (length libraries-loaded) num-libs)
             dir)))

(rb/load-all-in-directory (expand-file-name "lib" (expand-file-name "lisp" user-emacs-directory)))

;; =========================
;; Trailer
;; =========================

;; Allow users to provide an optional "init-local" containing personal settings
(require 'init-local nil t)

;;; init.el ends here
