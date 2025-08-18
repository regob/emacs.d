;;; init.el --- Emacs init file  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; ====================================
;; Initialize package manager and paths
;; ====================================

;; Pre-config local settings, for setting paths, etc needed for setup.
(require 'init-local-pre nil t)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; always load .el if newer than .elc
(setq load-prefer-newer t)

;; ====================================
;; General setup
;; ====================================

;; Setup load path for libraries
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((default-directory (expand-file-name "lisp/external" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;; save customizations to custom.el, if exists (which is git-ignored) instead of init.el
(setq custom-file (locate-user-emacs-file "custom.el"))

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

(define-prefix-command 'rb-help-keymap)
(global-set-key (kbd "C-c 7") 'rb-help-keymap)

;; ----------------------------------------------------------------------------
;; Misc
;; ----------------------------------------------------------------------------

(global-set-key (kbd "M-o") 'other-window)
;; Ctrl+tab similarly as in browsers/vscode
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)

;; ----------------------------------------------------------------------------
;; Load libraries
;; ----------------------------------------------------------------------------

(require 'init-appearance nil nil)
(require 'init-completion nil nil)
(require 'init-consult nil nil)
(require 'init-dired nil nil)
(require 'init-editing-utils nil nil)
(require 'init-eglot nil nil)
(require 'init-flymake nil nil)
(require 'init-help nil nil)
(require 'init-html nil nil)
(require 'init-ibuffer nil nil)
(require 'init-lisp nil nil)
(require 'init-minibuffer nil nil)
(require 'init-org nil nil)
(require 'init-project nil nil)
(require 'init-python nil nil)
(require 'init-sessions nil nil)
(require 'init-sh nil nil)
(require 'init-utils nil nil)
(require 'init-vc nil nil)

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
