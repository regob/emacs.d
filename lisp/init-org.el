;;; init-org.el --- Org mode and plugins -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package org
  :init
  (setq org-startup-truncated nil)
  :bind
  (:map org-mode-map ("C-c C-x t" . 'org-table-create))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python t)))
  )

;; Create anki flashcards from org sections
(use-package org-anki
  :after org
  :config
  (customize-set-variable 'org-anki-default-deck "dump")
  )


(provide 'init-org)

;;; init-org.el ends here
