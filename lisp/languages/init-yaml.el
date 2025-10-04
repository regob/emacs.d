;;; init-yaml.el --- Yaml mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if-treesit
    (use-package yaml-ts-mode
      :ensure nil
      :init
      (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
      ))


(provide 'init-yaml)

;;; init-yaml.el ends here
