;;; init-org.el --- Org mode and plugins -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package org
  :bind
  (:map org-mode-map ("C-c C-x t" . 'org-table-create))

  :commands org-mode
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (emacs-lisp . t)
     (plantuml . t)
     (python . t)
     (shell . t)
     (sql . t)))

  (setq-default org-export-coding-system 'utf-8
                org-confirm-babel-evaluate nil
                org-startup-truncated nil)
  )

;; Create anki flashcards from org sections
(use-package org-anki
  :after org
  :config
  (customize-set-variable 'org-anki-default-deck "dump")
  )


;; Following configs taken from:
;; https://github.com/purcell/emacs.d/blob/master/lisp/init-org.el

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(defvar rb-org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key rb-org-global-prefix-map (kbd "j") 'org-clock-goto)
(define-key rb-org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key rb-org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key rb-org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") rb-org-global-prefix-map)


(provide 'init-org)

;;; init-org.el ends here
