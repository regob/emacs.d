;;; init-sessions.el ---  Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package session
  :ensure (:source "MELPA")
  :config
  (session-initialize)
  (setq session-save-file (locate-user-emacs-file ".session"))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)
  (add-hook 'delete-frame-functions (lambda (_) (session-save-session)))
  )

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode)
  (add-hook 'delete-frame-functions (lambda (_) (recentf-save-list))))


(provide 'init-sessions)

;;; init-sessions.el ends here
