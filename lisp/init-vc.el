;;; init-vc.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Git integration
(use-package magit)
(use-package transient
  :ensure (:source "MELPA")
  )
;; Magit requires seq>=2.24
(use-package seq
  :ensure (:source "MELPA")
  )


;; Git changes highlighted on the fringe
(use-package diff-hl
  :config
  (add-hook 'elpaca-after-init-hook #'global-diff-hl-mode)
  ;; :hook
  ;; ((prog-mode . diff-hl-mode)
  ;;  (org-mode . diff-hl-mode))
  )

;; Git config file editing
(use-package git-modes)

(use-package smerge-mode
  :ensure nil
  :config
  (setq smerge-command-prefix "\C-cv"))

(provide 'init-vc)

;;; init-vc.el ends here

