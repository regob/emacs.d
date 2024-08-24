;;; init-editing-utils.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; indent with spaces
(setq-default indent-tabs-mode  nil)
(setq-default tab-width 4)

;; modes for navigating super_words and CamelCase words
(setq global-superword-mode t)
;; (setq global-subword-mode t)

;; visual line mode enabled, without word wrapping
(setq visual-line-mode t)
(setq word-wrap nil)


;;; Some settings taken from better-defaults

;; zap-up-to-char instead of zap-to-char
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Swap regex and standard isearch
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


(setq save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      require-final-newline t
      )

(electric-pair-mode -1)
(electric-indent-mode -1) ; TODO: still gets turned on in cc-mode

;; Automatic parenthesis handling
(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook
  (
   (css-mode . smartparens-mode)
   (prog-mode . smartparens-mode)
   (org-mode . smartparens-mode)
   (ielm . smartparens-mode))
  )

(use-package multiple-cursors
  :defer nil
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

;; Enable auto-revert-mode
(global-auto-revert-mode t)


;; default to utf-8 as on windows sometimes latin-1 is detected?
(if (eq system-type 'windows-nt)
    (progn
      (set-default-coding-systems 'utf-8)
      (setq-default buffer-file-coding-system 'utf-8-unix))
  )

;; https://www.masteringemacs.org/article/script-files-executable-automatically
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

;; ----------------------------------------------------------------------------
;; Macro config
;; ----------------------------------------------------------------------------

(define-prefix-command 'rb-macro-keymap)
(global-set-key (kbd "C-c m") 'rb-macro-keymap)
(bind-key (kbd "s") #'start-kbd-macro 'rb-macro-keymap)
(bind-key (kbd "e") #'end-kbd-macro 'rb-macro-keymap)
(bind-key (kbd "q") #'kbd-macro-query 'rb-macro-keymap)
(bind-key (kbd "r") #'call-last-kbd-macro 'rb-macro-keymap)
(bind-key (kbd "m") #'edit-last-kbd-macro 'rb-macro-keymap)


(provide 'init-editing-utils)

;;; init-editing-utils.el ends here
