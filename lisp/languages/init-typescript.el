;;; init-typescript.el --- Typescript support (using treesit) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when-treesit
 (use-package typescript-ts-mode
   :ensure nil
   :init
   (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
   ))


(provide 'init-typescript)

;;; init-typescript.el ends here
