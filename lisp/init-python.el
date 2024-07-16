;;; init-python.el --- Python scripts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package python
  :init
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (add-hook 'python-mode-hook #'electric-indent-local-mode)

  ;; set compile command with current file name
  (add-hook 'python-mode-hook #'(lambda ()
                                  (set (make-local-variable 'compile-command)
                                       (concat "python3 " buffer-file-name))))

  :commands python-mode
  :interpreter ("python3" . python-mode)
  ;;(python-environment-virtualenv (quote ("python3.8" "-m" "venv")))
  )


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright))))


(provide 'init-python)

;;; init-python.el ends here
