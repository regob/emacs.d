;;; init-windows.el --- Emacs window movement/handling -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:




;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :init
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  (global-set-key (kbd "C-x o") 'switch-window)
  (global-set-key (kbd "M-o") 'switch-window)
  )


(provide 'init-windows)

;;; init-windows.el ends here
