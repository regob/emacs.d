;;; init-markup.el --- Markup language config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :ensure (:source "MELPA")
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  )

(use-package markdown-mode
  :ensure (:source "MELPA")
  :config
  (setq markdown-command "pandoc")
  :custom-face
  (markdown-code-face ((t (:inherit org-code))))
  )
(use-package csv-mode
  :ensure (:source "ELPA")
  )

(provide 'init-markup)

;;; init-markup.el ends here
