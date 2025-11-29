;;; init-defaults.el --- Default setting overrides -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defalias 'view-emacs-news 'ignore)
(defalias 'describe-gnu-project 'ignore)

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

;; Backup/autosave
(setq make-backup-files nil)
(setq auto-save-default t)
(setq auto-save-interval 100)
(setq auto-save-timeout 5)

;; always ask before killing emacs (does not hold for emacsclient though)
(setq confirm-kill-emacs 'yes-or-no-p)

;; some performance settings
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 50000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; remove scratch message
(setq initial-scratch-message "")

;; indent with spaces
(setq-default indent-tabs-mode  nil)
(setq-default tab-width 4)
(setq-default fill-column 120)

;; show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; turn off auto fill
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

;; visual line mode enabled, without word wrapping
(setq visual-line-mode t)
(setq word-wrap nil)

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


(provide 'init-defaults)

;;; init-defaults.el ends here
