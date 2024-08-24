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



(use-package org
  :bind
  (:map org-mode-map ("C-c C-x t" . 'org-table-create))
  (:map global-map ("C-c l" . 'org-store-link))
  (:map global-map ("C-c c" . 'org-capture))
  (:map global-map ("C-c a" . 'org-agenda))

  :config
  (push 'org-habit org-modules)
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
        org-startup-truncated nil
        )
  
  ;; unbind some conflicting keys
  (unbind-key "C-'"  'org-mode-map)

  (defvar rb-org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

  ;; ----------------------------------------------------------------------------
  ;; Org refile
  ;; ----------------------------------------------------------------------------

  (defvar rb/org-refile-extra-list (list)
    "List of projects containing org files. Used for setting refiling targets")

  ;; Targets include this file and any file contributing to the agenda - up to 3 levels deep
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3)))
  
  ;; Save buffers after refiling
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  (defun rb/org-refile-extra ()
    "Refile with extra targets included besides agenda files."
    (interactive)
    (let ((org-refile-targets
           (quote ((nil :maxlevel . 3)
                   (org-agenda-files :maxlevel . 3)
                   (rb/org-refile-extra-list :maxlevel . 3)))))
      (org-refile))
    )

  (bind-key (kbd "w") #'rb/org-refile-extra 'rb-org-global-prefix-map)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; ----------------------------------------------------------------------------
  ;; org todo and agenda setup
  ;; ----------------------------------------------------------------------------

  (setq org-agenda-sticky t
        org-agenda-window-setup 'current-window
        org-log-into-drawer t
        )

  (setq org-stuck-projects
        '("/PROJ" ("NEXT")))

  (setq org-tag-alist
        '(("INPROGRESS" . ?i)
          ("MEETING" . ?m)
          ("NOTE" . ?n)
          ("CANCELLED" . ?c)
          ("REFILE" . ?r)))

  ;; Following configs adapted from:
  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-org.el
  ;; and https://doc.norang.ca/org-mode.html

  (setq org-capture-templates
        `(("t" "todo" entry (file "") ; "" => `org-default-notes-file'
           "* TODO %?\n%U\n" :clock-resume t)
          ("m" "meeting" entry (file "")
           "* MEETING Call %<%m-%d %H:%M> :MEETING:\n%U\n%?" :clock-resume t)
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
                  (stuck "-CANCELLED"
                         ((org-agenda-overriding-header "Stuck projects")))
                  (tags-todo "INPROGRESS/!"
                             ((org-agenda-overriding-header "Tasks in progress")))
                  (tags-todo "-CANCELLED-INPROGRESS/NEXT"
                             ((org-agenda-overriding-header "Project next tasks (backlog)")))
                  (tags-todo "-CANCELLED-INPROGRESS/TODO"
                             ((org-agenda-overriding-header "Standalone tasks (backlog)")
                              (org-agenda-skip-function 'rb/skip-project-tasks)))
                  (tags-todo "-CANCELLED/HOLD|WAITING|DELEGATED"
                             ((org-agenda-overriding-header "Waiting tasks"))))
                 )
                nil)))

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t) ("INPROGRESS"))
                ("WAITING" ("INPROGRESS"))
                ("HOLD" ("INPROGRESS"))
                (done ("INPROGRESS") ("HOLD"))
                ("TODO" ("CANCELLED"))
                ("NEXT" ("CANCELLED"))
                ("DONE" ("INPROGRESS") ("CANCELLED")))))

  ;; org agenda helper functions
  ;; adapted from: https://doc.norang.ca/org-mode.html#Projects

  (defun rb/is-project-p ()
    "Is the org subtree currently at point a project?"
    (let ((todo-state (org-get-todo-state)))
      (eq (read todo-state) 'PROJ)
      )
    )

  (defun rb/is-subtask-p ()
    "Any task which is a subtask of another project"
    (let ((is-subproject)
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (while (and (not is-subproject) (org-up-heading-safe))
          (when (eq (nth 2 (org-heading-components)) "PROJ")
            (setq is-subproject t))))
      (and is-a-task is-subproject)))
  
  (defun rb/skip-project-tasks ()
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((rb/is-project-p)
          subtree-end)
         ((org-is-habit-p)
          subtree-end)
         ((rb/is-subtask-p)
          subtree-end)
         (t
          nil)))))
  


  ;; ----------------------------------------------------------------------------
  ;; Org clocking
  ;; ----------------------------------------------------------------------------

  (setq org-clock-into-drawer t
        org-clock-out-remove-zero-time-clocks t
        )

  (define-key rb-org-global-prefix-map (kbd "j") 'org-clock-goto)
  (define-key rb-org-global-prefix-map (kbd "l") 'org-clock-in-last)
  (define-key rb-org-global-prefix-map (kbd "i") 'org-clock-in)
  (define-key rb-org-global-prefix-map (kbd "o") 'org-clock-out)
  (define-key global-map (kbd "C-c o") rb-org-global-prefix-map)
  )

(use-package anki-editor
  :ensure (:host github :repo "anki-editor/anki-editor" :branch "master")
  )

(provide 'init-org)

;;; init-org.el ends here
