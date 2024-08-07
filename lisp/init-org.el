;;; init-org.el --- Org mode and plugins -*- lexical-binding: t -*-
;;; Commentary:
;; Org config with a basic GTD and agenda setup, and anki integration
;; for flashcards.
;; 
;; Configure the following in init-local.el:
;; `org-agenda-files': directories containing the org notes.
;; `org-default-notes-file': Default file for org capture notes.  This file
;; should have `#+FILETAGS: REFILE' at the top.
;;; Code:

(defvar rb/org-refile-extra-list (list)
  "List of projects containing org files. Used for setting refiling targets")

(use-package org
  :bind
  (:map org-mode-map ("C-c C-x t" . 'org-table-create))
  (:map global-map ("C-c l" . 'org-store-link))
  (:map global-map ("C-c c" . 'org-capture))
  (:map global-map ("C-c a" . 'org-agenda))

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

  ;; ----------------------------------------------------------------------------
  ;; org todo and agenda setup
  ;; ----------------------------------------------------------------------------


  ;; Targets include this file and any file contributing to the agenda - up to 3 levels deep
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)
          (rb/org-refile-extra-list :maxlevel . 3)))
  
  ;; Save buffers after refiling
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  (setq org-agenda-sticky t
        org-agenda-window-setup 'current-window
        org-log-into-drawer t
        )

  (setq org-stuck-projects
        '("PROJ" ("NEXT")))

  ;; Following configs adapted from:
  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-org.el
  ;; and https://doc.norang.ca/org-mode.html

  (setq org-capture-templates
        `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
           "* TODO %?\n%U\n" :clock-resume t)
          ("m" "meeting" entry (file "")
           "* MEETING %? :MEETING:\n%U\n" :clock-resume t)
          ("r" "respond" entry (file "")
           "* NEXT Respond to %?\n%U\n" :clock-resume t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n" :clock-resume t)
          )
        )


  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                (sequence "PROJ(p)" "|" "DONE(d!/!)")
                (sequence "|" "MEETING(m)" "(q)") ; the empty keyword (q) is for removing todo tags
                (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)"))
               ))

  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)

  (setq org-agenda-custom-commands
        (quote (("h" "Agenda and Home-related tasks"
                 ((agenda "")
                  (tags-todo "home")
                  (tags "garden")))
                ("o" "Agenda and Office-related tasks"
                 ((agenda "")
                  (tags-todo "work")
                  (tags "office"))))))

  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        (quote (("N" "Notes" tags "NOTE"
                 ((org-agenda-overriding-header "Notes")
                  (org-tags-match-list-sublevels t)))
                (" " "Agenda"
                 ((agenda ""
                          ((org-agenda-span 'day)))
                  (tags "REFILE"
                        ((org-agenda-overriding-header "Tasks to Refile")
                         (org-tags-match-list-sublevels nil)))
                  (tags-todo "-CANCELLED/!"
                             ((org-agenda-overriding-header "Stuck Projects")
                              (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                              (org-agenda-sorting-strategy
                               '(category-keep))))
                  (tags "-REFILE/"
                        ((org-agenda-overriding-header "Tasks to Archive")
                         (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                         (org-tags-match-list-sublevels nil))))
                 nil))))


  ;; ----------------------------------------------------------------------------
  ;; Org clocking
  ;; ----------------------------------------------------------------------------

  (setq org-clock-into-drawer t
        org-clock-out-remove-zero-time-clocks t
        )

  (defvar rb-org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

  (define-key rb-org-global-prefix-map (kbd "j") 'org-clock-goto)
  (define-key rb-org-global-prefix-map (kbd "l") 'org-clock-in-last)
  (define-key rb-org-global-prefix-map (kbd "i") 'org-clock-in)
  (define-key rb-org-global-prefix-map (kbd "o") 'org-clock-out)
  (define-key global-map (kbd "C-c o") rb-org-global-prefix-map)
  )

;; Create anki flashcards from org sections
(use-package org-anki
  :after org
  :config
  (customize-set-variable 'org-anki-default-deck "dump")
  )






(provide 'init-org)

;;; init-org.el ends here
