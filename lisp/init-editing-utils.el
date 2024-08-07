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


(require 'apropos)
(setq apropos-do-all t)

(setq save-interprogram-paste-before-kill t

      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      frame-inhibit-implied-resize t
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
   (org-mode . smartparens-mode))
  )


;; Show key suggestions when typing a key chord
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

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



(provide 'init-editing-utils)

;;; init-editing-utils.el ends here
