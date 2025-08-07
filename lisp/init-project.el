;;; init-project.el --- Project config (projectile) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure nil
  :diminish projectile-mode
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  (add-to-list 'projectile-globally-ignored-directories ".direnv")
  :config
  (add-hook 'prog-mode-hook #'(lambda () (projectile-mode +1)))
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
