;;; init.el --- Emacs init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


;; ----------------------------------------------------------------------------
;; Set up load paths
;; ----------------------------------------------------------------------------

;; Setup load path for libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;; save customizations to custom.el, if exists (which is git-ignored) instead of init.el
(setq custom-file (locate-user-emacs-file "custom.el"))


;; ----------------------------------------------------------------------------
;; Set up local machine config
;; ----------------------------------------------------------------------------

(defvar rb/host-type nil
  "Either 'home or 'work depending on host.")

(defvar rb/use-treesit t
  "Use tree-sitter or not")

(defmacro when-home (&rest body)
  "Evaluate BODY only when `rb/host-type` is 'home."
  `(when (eq rb/host-type 'home)
     ,@body))

(defmacro when-work (&rest body)
  "Evaluate BODY only when `rb/host-type` is 'work."
  `(when (eq rb/host-type 'work)
     ,@body))

(defmacro when-treesit (&rest body)
  "Evaluate BODY only when rb/use-treesit is t."
  `(when rb/use-treesit
     ,@body))

(defmacro if-treesit (body-true &rest body-false)
  "Evaluate BODY-TRUE or BODY-FALSE depending on rb/use-treesit."
  (declare (indent 1))
  `(if rb/use-treesit
       ,body-true
     (progn ,@body-false)))


;; Pre-config local settings, for setting paths, etc needed for setup.
(require 'init-local-pre nil nil)



;; Verify that mandatory host-type is set up by now
(unless (and (boundp 'rb/host-type)
             (memq rb/host-type '(home work)))
  (error "Invalid or undefined `rb/host-type`: %S (expected 'home or 'work)"
         rb/host-type))

;; ----------------------------------------------------------------------------
;; Initialize package archives and package manager
;; ----------------------------------------------------------------------------


(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(when-home
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; always load .el if newer than .elc
(setq load-prefer-newer t)

;; ====================================
;; General setup
;; ====================================

;; Set autosave directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq backup-by-copying t)

;; always ask before killing emacs (does not hold for emacsclient though)
(setq confirm-kill-emacs 'yes-or-no-p)

;; some performance settings (partly for lsp-mode)
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;; (setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; remove scratch message
(setq initial-scratch-message "")

;; ----------------------------------------------------------------------------
;; Some "global" keymaps
;; ----------------------------------------------------------------------------

(require 'init-keys nil nil)

;; ----------------------------------------------------------------------------
;; Init general modules
;; ----------------------------------------------------------------------------

(require 'init-appearance nil nil)
(require 'init-completion nil nil)
(require 'init-consult nil nil)
(require 'init-dired nil nil)
(require 'init-editing-utils nil nil)
(require 'init-eglot nil nil)
(require 'init-flymake nil nil)
(require 'init-help nil nil)
(require 'init-ibuffer nil nil)
(require 'init-minibuffer nil nil)
(require 'init-project nil nil)
(require 'init-sessions nil nil)
(use-package so-long
  :ensure nil
  :config
  (global-so-long-mode))
(require 'init-utils nil nil)
(require 'init-vc nil nil)

(when-treesit
 (require 'init-treesit nil nil))

;; Language modes
(require 'init-dockerfile nil nil)
(require 'init-html nil nil)
(require 'init-java nil nil)
(require 'init-cc nil nil)
(require 'init-javascript nil nil)
(require 'init-json nil nil)
(require 'init-lisp nil nil)
(require 'init-markdown nil nil)
(require 'init-org nil nil)
(require 'init-python nil nil)
(require 'init-sh nil nil)
(require 'init-typescript nil nil)
(require 'init-yaml nil nil)

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
