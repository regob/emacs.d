;;; init-sessions.el ---  Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package savehist
  :ensure nil
  :config
  (setq savehist-additional-variables
        '(project--list)))


(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 1000)
  (recentf-mode)
  (add-hook 'delete-frame-functions (lambda (_) (recentf-save-list))))


(provide 'init-sessions)

;;; init-sessions.el ends here
