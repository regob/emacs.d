;;; init-cc.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "linux")
  (setq-default c-basic-offset 4)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard
                                            "c++17")))
  (add-hook 'c-mode-common-hook (lambda () (c-toggle-hungry-state 1)))
  (add-hook 'c-mode-common-hook 'smartparens-mode)
  ;; (add-hook 'c-mode-common-hook #'aggressive-indent-mode)
  )

(provide 'init-cc)

;;; init-cc.el ends here
