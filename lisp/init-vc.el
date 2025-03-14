;;; init-vc.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Git integration
(use-package magit
  :ensure (:source "MELPA"))


;; Git changes highlighted on the fringe
(use-package diff-hl
  :config
  (add-hook 'elpaca-after-init-hook #'global-diff-hl-mode)
  )

;; Git config file editing
(use-package git-modes)

(use-package smerge-mode
  :ensure nil
  :init
  (setq smerge-command-prefix "\C-cv"))

(provide 'init-vc)

;;; init-vc.el ends here

