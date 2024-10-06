;;; init-shell.el --- Shell config in emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; do not add -i flag, instead source .bashrc from shell starting emacs
(setq shell-command-switch "-c")

(provide 'init-shell)

;;; init-shell.el ends here
