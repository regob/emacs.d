;;; init-vc.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Git integration
(use-package magit
  :ensure t

  )

(use-package smerge-mode
  :ensure nil
  :init
  (setq smerge-command-prefix "\C-cv"))

(provide 'init-vc)

;;; init-vc.el ends here

