;;; init-help.el --- Utilities for finding stuff  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Show transient key suggestions when typing a key chord
(use-package which-key
  :ensure t
  :config
  (which-key-mode))


(use-package apropos
  :ensure nil
  :config
  (setq apropos-do-all t))

;; Read manpages without man
(use-package woman
  :ensure nil
  :config
  (setq woman-fill-frame t)
  )

(use-package eldoc
  :ensure nil
  :config
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))

(provide 'init-help)

;;; init-help.el ends here
