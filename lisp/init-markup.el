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
  :bind (:map
         markdown-mode-map
         ("M-<up>" . markdown-move-up)
         ("M-<down>" . markdown-move-down))
  :config
  (setq markdown-command "pandoc")
  :custom-face
  (markdown-code-face ((t (:inherit org-code))))
  )
(use-package csv-mode
  :ensure (:source "ELPA")
  )


(use-package sql
  :ensure nil)

(provide 'init-markup)

;;; init-markup.el ends here
