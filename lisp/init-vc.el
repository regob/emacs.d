;;; init-vc.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Git integration
(use-package magit)

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

(provide 'init-vc)

;;; init-vc.el ends here

