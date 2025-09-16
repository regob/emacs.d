;;; init-project.el --- Project config (projectile) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun rb/project-ripgrep ()
  "Run ripgrep on a project"
  (interactive)
  (when (fboundp 'project-root)
      (consult-ripgrep (project-root (project-current t)) "")))

(defun rb/project-magit ()
  "Open magit status for the project"
  (interactive)
  (when (fboundp 'project-root)
      (magit-status (project-root (project-current t)))))

(use-package project
  :ensure nil
  :config
  ;; Add extra commands to project dispatch
  (keymap-set project-prefix-map "r" #'rb/project-ripgrep)
  (add-to-list 'project-switch-commands '(rb/project-ripgrep "Ripgrep") t)
  (keymap-set project-prefix-map "m" #'rb/project-magit)
  (add-to-list 'project-switch-commands '(rb/project-magit "Magit") t)
  )

(use-package compile
  :ensure nil
  :config
  ;; Switch to previous buffer when compilation is successful
  ;; https://stackoverflow.com/a/11059012/11579038
  (defun rb-bury-compile-buffer-if-successful (buffer string)
    "Bury a compilation buffer if succeeded without warnings."
    (when (and
           (buffer-live-p buffer)
           (string-match "compilation" (buffer-name buffer))
           (string-match "finished" string)
           )
      (run-with-timer 1 nil
                      (lambda (buf)
                        (bury-buffer buf)
                        (switch-to-prev-buffer (get-buffer-window buf) 'kill)
                        ;; delete window if it was opened by the compilation process
                        ;; (have two windows with the same buffer)
                        (when
                            (and (equal 2 (length (window-list)))
                                 (eq (window-buffer (car (window-list))) (window-buffer (cadr (window-list)))))
                          (delete-window (selected-window))
                          )
                        )
                      buffer)))
  (add-hook 'compilation-finish-functions 'rb-bury-compile-buffer-if-successful))

(provide 'init-project)

;;; init-project.el ends here
