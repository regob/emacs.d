;;; init-editing-utils.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; indent with spaces
(setq-default indent-tabs-mode  nil)
(setq-default tab-width 4)
(setq-default fill-column 120)

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
  :ensure t
  :pin nongnu
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
;; Indentation / parens
;; ----------------------------------------------------------------------------

(when-home
    (use-package aggressive-indent
      :ensure t
      :pin gnu
      :diminish
      :hook
      ((css-mode . aggressive-indent-mode))
      ((emacs-lisp-mode . aggressive-indent-mode))
      ))

(use-package electric
  :ensure nil
  :hook
  ((python-mode . electric-indent-local-mode)
   (cc-mode . electric-indent-local-mode)
   (c++-mode . electric-indent-local-mode)
   (sh-mode . electric-indent-local-mode)
   (yaml-mode . electric-indent-local-mode)
   (js-json-mode . electric-indent-local-mode)
   (emacs-lisp-mode . electric-indent-local-mode)

   (python-mode . electric-pair-local-mode)
   (emacs-lisp-mode . electric-pair-local-mode)
   (cc-mode . electric-pair-local-mode)
   (c++-mode . electric-pair-local-mode)
   (sh-mode . electric-pair-local-mode)
   )
  :config
  (electric-indent-mode -1)
  (electric-pair-mode -1))

(defun rb-insert-parens-around-sexp (arg)
  "Wrap the next ARG sexps in parentheses.
  If no ARG is given, wrap 1 sexp by default (instead of 0 in `insert-parentheses')."
  (interactive "P")
  (insert-parentheses (prefix-numeric-value (or arg 1))))

(global-set-key (kbd "M-(") #'rb-insert-parens-around-sexp)

;; ----------------------------------------------------------------------------
;; Whitespace cleanup
;; ----------------------------------------------------------------------------

(when-home
 (use-package whitespace-cleanup-mode
   :ensure t
   :pin melpa
   :diminish
   :hook ((prog-mode . whitespace-cleanup-mode)
          (conf-mode . whitespace-cleanup-mode))))

;; ----------------------------------------------------------------------------
;; Undo tree
;; ----------------------------------------------------------------------------

;; vundo seems to be less intrusive, easier to use than undo-tree
(when-home
 (use-package vundo
   :ensure t
   :pin gnu
   ))


;; ----------------------------------------------------------------------------
;; Grabbing lines
;; ----------------------------------------------------------------------------

(global-set-key (kbd "M-<up>") 'rb-grab-up)
(global-set-key (kbd "M-<down>") 'rb-grab-down)


(provide 'init-editing-utils)

;;; init-editing-utils.el ends here
