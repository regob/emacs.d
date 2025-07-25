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
  :ensure nil
  :bind
  (:map org-mode-map ("C-c C-x t" . 'org-table-create))
  (:map global-map ("C-c c" . 'org-capture))
  (:map global-map ("C-c a" . 'org-agenda))
  (:map global-map ("<f12>" . 'org-agenda))

  :config
  (push 'org-habit org-modules)
  (push 'org-protocol org-modules)
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

        ;; some visual settings
        org-startup-truncated nil
        org-imenu-depth 3
        org-startup-folded 'content
        org-startup-indented nil
        org-hide-leading-stars t
        )

  ;; unbind some conflicting keys
  (unbind-key "C-'"  'org-mode-map)

  (defvar rb-org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

  ;; insert link saved by e.g. org-protocol
  (bind-key "l" 'org-insert-link 'rb-org-global-prefix-map)
  (bind-key "s" 'org-store-link 'rb-org-global-prefix-map)

  ;; ----------------------------------------------------------------------------
  ;; Org refile
  ;; ----------------------------------------------------------------------------

  (defvar rb/org-refile-extra-list (list)
    "List of projects containing org files. Used for setting refiling targets")

  (defcustom rb/org-refile-depth 2
    "Depth of headings to appear as refiling targets in org-mode files."
    :type 'integer
    :group 'rb-group)

  (defun rb/init-refile-targets (depth)
    "Set org-refile-targets until a given depth."
    ;; Targets include this file and any file contributing to the agenda - up to n depth
    (setq org-refile-targets
          `((nil :maxlevel . ,depth)
            (org-agenda-files :maxlevel . ,depth)))
    )

  ;; Initialize org-refile-targets, and recompute when the depth changes
  (rb/init-refile-targets rb/org-refile-depth)
  (add-variable-watcher 'rb/org-refile-depth
                        (lambda (_ newval op _) (when (eq op 'set)
                                                  (rb/init-refile-targets newval))))

  ;; Save buffers after refiling
  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  (defun rb/org-refile-extra ()
    "Refile with extra targets included besides agenda files."
    (interactive)
    (let ((org-refile-targets
           (backquote ((nil :maxlevel . ,rb/org-refile-depth)
                       (org-agenda-files :maxlevel . ,rb/org-refile-depth)
                       (rb/org-refile-extra-list :maxlevel . ,rb/org-refile-depth)))))
      (org-refile))
    )

  (bind-key (kbd "w") #'rb/org-refile-extra 'rb-org-global-prefix-map)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)


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
          ("REFILE" . ?r)
          ("CARD" . ?a)))

  ;; Following configs adapted from:
  ;; https://github.com/purcell/emacs.d/blob/master/lisp/init-org.el
  ;; and https://doc.norang.ca/org-mode.html

  (setq org-capture-templates
        `(("t" "todo" entry (file "") ; "" => `org-default-notes-file'
           "* TODO %?\n%U\n" :clock-resume t)
          ("m" "meeting" entry (file "")
           "* MEETING Call %<%m-%d> :MEETING:\n%U\n%?" :clock-resume t)
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
                (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)"))
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
                ("C" "Cards" tags "CARD"
                 ((org-agenda-overriding-header "Cards")
                  (org-tags-match-list-sublevels t)))
                ("U" "Unsynced cards" tags "CARD-ANKI_NOTE_ID={.+}"
                 ((org-tags-match-list-sublevels t)))
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
                              (org-agenda-skip-function 'rb/skip-project-tasks)
                              (org-agenda-skip-function 'rb/skip-schedule-deadline)))
                  (tags-todo "-CANCELLED/HOLD|WAITING|DELEGATED"
                             ((org-agenda-overriding-header "Waiting tasks")
                              ))
                  (tags-todo "-CANCELLED/SOMEDAY"
                             ((org-agenda-overriding-header "Someday backlog")
                              )))
                 )
                )))

  (setq org-todo-state-tags-triggers
        (quote (("CANCELLED" ("CANCELLED" . t) ("INPROGRESS"))
                ("WAITING" ("INPROGRESS"))
                ("HOLD" ("INPROGRESS"))
                ("SOMEDAY" ("INPROGRESS"))
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
          (when (string-equal (nth 2 (org-heading-components)) "PROJ")
            (setq is-subproject t))))
      (and is-a-task is-subproject)))

  (defun rb/task-scheduled-or-deadline-p ()
    "Is this task scheduled or has deadline?"
    (let* ((scheduled-string (org-entry-get (point) "SCHEDULED"))
           (deadline-string (org-entry-get (point) "DEADLINE"))
           (recur-pattern "^.*\\(\\+\\+\\|\\.\\+\\).*$")
           (has-recurrent-schedule (and
                                    (stringp scheduled-string)
                                    (string-match-p recur-pattern scheduled-string)))
           (has-recurrent-deadline (and
                                    (stringp deadline-string)
                                    (string-match-p recur-pattern deadline-string))))
      (if (or has-recurrent-deadline has-recurrent-schedule)
          t
        nil))
    )

  (defun rb/skip-schedule-deadline ()
    "Skip scheduled or deadline tasks in agenda view."
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (rb/task-scheduled-or-deadline-p)
          subtree-end
        nil)))

  (defun rb/skip-project-tasks ()
    "Skip tasks which are under a project heading (PROJ)"
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
  (define-key rb-org-global-prefix-map (kbd "b") 'org-clock-in-last)
  (define-key rb-org-global-prefix-map (kbd "i") 'org-clock-in)
  (define-key rb-org-global-prefix-map (kbd "o") 'org-clock-out)


  (defun rb/org-start-pomodoro-work ()
    "Start an org pomodoro timer for working (25 minutes)."
    (interactive)
    (org-timer-set-timer 25))

  (defun rb/org-start-pomodoro-rest ()
    "Start an org pomodoro timer for resting (5 minutes)."
    (interactive)
    (org-timer-set-timer 5))

  (defvar rb-org-pomodoro-keymap (make-sparse-keymap)
    "Keymap for org pomodoro commands.")

  (keymap-set rb-org-pomodoro-keymap "w" 'rb/org-start-pomodoro-work)
  (keymap-set rb-org-pomodoro-keymap "r" 'rb/org-start-pomodoro-rest)
  (keymap-set rb-org-pomodoro-keymap "c" 'org-timer-stop)

  (keymap-set global-map "C-c o" rb-org-global-prefix-map)
  (keymap-set rb-org-global-prefix-map "p" rb-org-pomodoro-keymap)


  ;; ----------------------------------------------------------------------------
  ;; Org latex
  ;; ----------------------------------------------------------------------------

  (setq org-startup-with-latex-preview nil)
  (setq org-startup-with-link-previews t)

  ;; ----------------------------------------------------------------------------
  ;; Misc settings
  ;; ----------------------------------------------------------------------------

  ;; save all org buffers at xx:59 every hour
  (run-at-time "00:59" 3600 'org-save-all-org-buffers)

  (keymap-set rb-org-global-prefix-map "c" 'org-cut-subtree)

  )

;; ----------------------------------------------------------------------------
;; Anki editor integration
;; ----------------------------------------------------------------------------


(defun rb/org-insert-anki-card ()
  "Insert a card after the current heading in Org mode."
  (interactive)
  (org-end-of-subtree)  ;; Move to the end of the current heading section
  (org-insert-heading-after-current)  ;; Equivalent to C-u M-RET
  (save-excursion
    (insert " :CARD:"))  ;; Replace with your template
  (org-show-entry))

(use-package anki-editor
  :ensure (:host github :repo "anki-editor/anki-editor" :branch "master")
  :config
  (define-prefix-command 'rb-anki-keymap)
  (keymap-set rb-anki-keymap "p" #'anki-editor-push-note-at-point)
  (keymap-set rb-anki-keymap "d" #'anki-editor-delete-note-at-point)
  (keymap-set rb-anki-keymap "s" #'anki-editor-sync-collection)
  (keymap-set rb-anki-keymap "i" #'rb/org-insert-anki-card)
  (keymap-set rb-org-global-prefix-map "a" rb-anki-keymap)
  ;; ignore some tags used in org agenda
  (setq anki-editor-ignored-org-tags
        (append '("INPROGRESS" "MEETING" "NOTE" "CANCELLED" "REFILE" "CARD")
                anki-editor-ignored-org-tags))
  )


(provide 'init-org)

;;; init-org.el ends here
