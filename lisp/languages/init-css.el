;;; init-css.el --- CSS editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package css-mode
  :ensure nil
  )

(when-treesit
 (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
 )

(provide 'init-css)

;;; init-css.el ends here
