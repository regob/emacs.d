;;; init-project.el --- Project config (projectile) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  (add-to-list 'projectile-globally-ignored-directories ".direnv")
  :config
  (add-hook 'prog-mode-hook #'(lambda () (projectile-mode +1)))
  )


(provide 'init-project)

;;; init-project.el ends here
