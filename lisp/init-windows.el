;;; init-windows.el --- Emacs window movement/handling -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package switch-window
  :ensure (:source "MELPA")
  :config
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  (global-set-key (kbd "C-x o") 'other-window)
  ;; Make "M-o" prompt for a target window when there are more than 2
  (global-set-key (kbd "M-o") 'switch-window)
  ;; Ctrl+tab similarly as in browsers/vscode
  (global-set-key (kbd "C-<tab>") 'next-buffer)
  (global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)
  )

(provide 'init-windows)

;;; init-windows.el ends here
