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

(use-package eldoc
  :diminish
  :ensure nil
  :config
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

;; ----------------------------------------------------------------------------
;; Casual suite - transient interfaces to commands
;; ----------------------------------------------------------------------------

(use-package casual-calc
  :ensure (:source "MELPA")
  :bind (:map
         calc-mode-map
         ("C-c t" . 'casual-calc-tmenu)
         :map
         calc-alg-map
         ("C-c t" . 'casual-calc-tmenu)))

(use-package casual-ibuffer
  :ensure (:source "MELPA")
  :bind (:map
	     ibuffer-mode-map
	     ("C-c t" . 'casual-ibuffer-tmenu))
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

(provide 'init-help)

;;; init-help.el ends here
