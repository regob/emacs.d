;;; init-help.el --- Utilities for finding stuff  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Show transient key suggestions when typing a key chord
(use-package which-key
  :diminish which-key-mode
  :ensure t
  :config
  (which-key-mode))


(use-package apropos
  :ensure nil
  :config
  (setq apropos-do-all t))

;; Read manpages without man
(use-package woman
  :ensure nil
  :config
  (setq woman-fill-frame t)
  )

;; Neat documentation features
(use-package eldoc
  :diminish
  :ensure nil
  :config
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;; ----------------------------------------------------------------------------
;; Casual suite - transient interfaces to commands
;; ----------------------------------------------------------------------------

(use-package casual
  :ensure (:source "MELPA")
  :config
  (with-eval-after-load "calc-alg"
    (keymap-set calc-alg-map "C-c t" #'casual-calc-tmenu))
  (with-eval-after-load "calc"
    (keymap-set calc-mode-map "C-c t" #'casual-calc-tmenu))
  (with-eval-after-load "calendar"
    (keymap-set calendar-mode-map "C-c t" #'casual-calendar))
  (with-eval-after-load "dired"
    (keymap-set dired-mode-map "C-c t" #'casual-dired-tmenu))  
  (with-eval-after-load "ibuffer"
    (keymap-set ibuffer-mode-map "C-c t" #'casual-ibuffer-tmenu))
  (with-eval-after-load "re-builder"
    (progn
      (keymap-set reb-mode-map "C-c t" #'casual-re-builder-tmenu)
      (keymap-set reb-lisp-mode-map "C-c t" #'casual-re-builder-tmenu)))  
  )

;; ----------------------------------------------------------------------------
;; Dictionary
;; ----------------------------------------------------------------------------

(use-package dictionary
  :ensure nil
  :custom
  (dictionary-use-single-buffer t)
  :config
  (bind-key (kbd "d") #'dictionary-lookup-definition 'rb-user-keymap))

;; ----------------------------------------------------------------------------
;; GPTel - LLM interface for emacs
;; ----------------------------------------------------------------------------

(use-package gptel
  :ensure (:source "MELPA")
  :config
  (setq gptel-api-key (lambda () (getenv "OPENAI_API_KEY")))
  (setq gptel-model "gpt-4o-mini")
  )

(provide 'init-help)

;;; init-help.el ends here
