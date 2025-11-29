;;; init-cc.el --- C and C++ modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package cc-mode
  :ensure nil
  :config
  (setq c-default-style "user")
  (setq c-basic-offset 4)
  (setq c-default-offset 4)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard
                                            "c++17")))
  )

(provide 'init-cc)

;;; init-cc.el ends here
