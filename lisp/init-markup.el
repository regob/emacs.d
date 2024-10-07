;;; init-markup.el --- Markup language config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
  )

(use-package markdown-mode
  :config
  (setq markdown-command "pandoc")
  :custom-face
  (markdown-code-face ((t (:inherit org-code))))
  )
(use-package csv-mode)

(provide 'init-markup)

;;; init-markup.el ends here
