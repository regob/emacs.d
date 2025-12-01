;;; init-shell.el --- Shell config in emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; do not add -i flag, instead source .bashrc from shell starting emacs
(setq shell-command-switch "-c")

;; Set cat as pager, as less does not work well in eshell
(setenv "PAGER" "cat")

(use-package envrc
  :ensure nil
  :config
  (envrc-global-mode)
  )

(provide 'init-shell)

;;; init-shell.el ends here
