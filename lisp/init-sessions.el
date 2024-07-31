;;; init-sessions.el ---  Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:





(use-package session
  ;; :init
  ;; (add-hook 'elpaca-after-init-hook 'session-initialize)
  
  :ensure t
  :config
  (session-initialize)
  (setq session-save-file (locate-user-emacs-file ".session"))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)
  (add-hook 'delete-frame-functions 'session-save-session)
  )




(provide 'init-sessions)

;;; init-sessions.el ends here
