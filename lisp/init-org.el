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

  
  (setq org-export-coding-system 'utf-8
        org-confirm-babel-evaluate nil
        org-startup-truncated nil)
  (global-set-key (kbd "C-c c") 'org-capture)
  
  ;; org todo/agenda setup
  (define-key global-map (kbd "C-c a") 'org-agenda)
  (setq org-agenda-sticky t
        org-agenda-window-setup 'current-window
        org-log-into-drawer t)
  

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

(defvar rb-org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key rb-org-global-prefix-map (kbd "j") 'org-clock-goto)
(define-key rb-org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key rb-org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key rb-org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") rb-org-global-prefix-map)



(setq org-capture-templates
      `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
         "* NEXT %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        )
      org-default-notes-file (concat (file-name-directory user-init-file) ".notes"))


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)" "(q)")))
      )

(provide 'init-org)

;;; init-org.el ends here
