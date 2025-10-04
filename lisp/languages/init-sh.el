;;; init-sh.el --- Shell/bash script editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package sh-script
  :ensure nil
  :config
  (setq sh-basic-offset 4)
  )

(if-treesit
    (use-package bash-ts-mode
      :ensure nil
      :init
      (add-to-list 'auto-mode-alist '("\\.bash\\'" . bash-ts-mode))
      (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
      ))

(provide 'init-sh)

;;; init-sh.el ends here
