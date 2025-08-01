;;; init-editing-utils.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; indent with spaces
(setq-default indent-tabs-mode  nil)
(setq-default tab-width 4)

;; show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; turn off auto fill
(remove-hook 'text-mode-hook 'turn-on-auto-fill)


;; mode for navigating super_words
(use-package subword
  :ensure nil
  :diminish superword-mode
  :config
  (customize-set-variable 'global-subword-mode t)
  )


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

;; bind C-x C-d to duplicate-line, since
;; I don't use list-directory that much
(global-set-key (kbd "C-x C-d") 'duplicate-line)

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

;; ----------------------------------------------------------------------------
;; Indentation
;; ----------------------------------------------------------------------------


(use-package aggressive-indent
  :diminish
  :ensure (:source "MELPA")
  :hook
  ((css-mode . aggressive-indent-mode))
  ((emacs-lisp-mode . aggressive-indent-mode))
  )

(use-package electric
  :ensure nil
  :hook
  ((kotlin-mode . electric-indent-local-mode)
   (python-mode . electric-indent-local-mode)
   (cc-mode . electric-indent-local-mode)
   (sh-mode . electric-indent-local-mode)
   (yaml-mode . electric-indent-local-mode)
   (js-json-mode . electric-indent-local-mode))
  :config
  (electric-indent-mode -1)
  (electric-pair-mode -1))

;; ----------------------------------------------------------------------------
;; Whitespace cleanup
;; ----------------------------------------------------------------------------

(use-package whitespace-cleanup-mode
  :diminish
  :ensure (:source "MELPA")
  :hook ((prog-mode . whitespace-cleanup-mode)
         (conf-mode . whitespace-cleanup-mode)))

;; ----------------------------------------------------------------------------
;; Undo tree
;; ----------------------------------------------------------------------------

(use-package undo-tree
  :diminish undo-tree-mode
  :ensure (:source "MELPA")
  :config
  (global-undo-tree-mode)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  )


;; ----------------------------------------------------------------------------
;; Grabbing lines
;; ----------------------------------------------------------------------------

(global-set-key (kbd "M-<up>") 'rb-grab-up)
(global-set-key (kbd "M-<down>") 'rb-grab-down)


(provide 'init-editing-utils)

;;; init-editing-utils.el ends here
