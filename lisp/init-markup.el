;;; init-markup.el --- Markdown and other markup languages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package markdown-mode
  :ensure t
  :pin nongnu
  )

(use-package yaml-ts-mode
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  )

(provide 'init-markup)

;;; init-markup.el ends here
