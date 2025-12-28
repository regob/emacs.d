;;; init-vc.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Git integration
(use-package magit
  :ensure t
  )

;; Git changes highlighted on the fringe
(use-package diff-hl
  :ensure t
  :pin gnu
  :config
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  )

(use-package smerge-mode
  :ensure nil
  :custom
  (smerge-command-prefix "\C-cv")
  )

(provide 'init-vc)

;;; init-vc.el ends here

