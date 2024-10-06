;;; init-dired.el --- dired  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil))


(provide 'init-dired)

;;; init-dired.el ends here
