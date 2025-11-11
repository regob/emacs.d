;;; init-dired.el --- dired  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq delete-by-moving-to-trash t)
  ;; Sort versions appropriately (v), and human output (h)
  (setq dired-listing-switches "-alvh")
  )

(use-package dired-x
  :ensure nil
  :config
  (global-set-key (kbd "C-x C-f") 'dired-x-find-file)
  (global-set-key (kbd "C-x 4 f") 'dired-x-find-file-other-window)
  )

(when-home
    (use-package dired-sidebar
      :ensure t
      :pin melpa
      :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
      :commands (dired-sidebar-toggle-sidebar)))

(provide 'init-dired)

;;; init-dired.el ends here
