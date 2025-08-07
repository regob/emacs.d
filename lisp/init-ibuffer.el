;;; init-ibuffer.el --- Builtin interactive buffer mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ibuffer
  :ensure nil
  :config
  (global-set-key (kbd "C-x C-b") #'ibuffer)
  ;; collision with switch-buffer
  (keymap-unset ibuffer-mode-map "M-o" t)
  )


(provide 'init-ibuffer)

;;; init-ibuffer.el ends here
