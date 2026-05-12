;;; init-sessions.el ---  Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;; session.el is not used anymore, since savehist provides the functionality to
;; save some variables.
;;; Code:

(use-package savehist
  :ensure nil
  :init
  (setq savehist-additional-variables
        '(project--list
          register-alist
          bookmark-alist
          search-ring
          regexp-search-ring
          compile-history
          ;; TODO: also manage named kbd macros in elisp file
          kmacro-ring
          last-kbd-macro))
  :config
  (savehist-mode)
  )


(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 1000)
  (add-to-list 'recentf-exclude "\\elpa")
  (recentf-mode)
  (add-hook 'delete-frame-functions (lambda (_) (recentf-save-list))))


(provide 'init-sessions)

;;; init-sessions.el ends here
