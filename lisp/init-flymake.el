;;; init-flymake.el --- Flymake linter/static analysis support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar rb--flymake-underline-enabled
  nil
  "Non-nil if flymake underlining is enabled.")

(defun rb--set-flymake-underline-enabled (enable)
  "Enable or disable underlining errors by flymake."
  (setq rb--flymake-underline-enabled enable)
  (if enable
      (custom-set-faces
       `(flymake-error ((t :underline (:style wave :color "#cc0000"))))
       `(flymake-warning ((t (:underline (:color "#aa5500" :style wave :position nil)))))
       `(flymake-note ((t :underline (:style line :color "#008800")))))
    (custom-set-faces
     `(flymake-error ((t :underline nil)))
     `(flymake-warning ((t (:underline nil))))
     `(flymake-note ((t :underline nil))))
    ))

(defun rb/toggle-flymake-underline ()
  "Toggle enabling the underlining of errors by flymake."
  (interactive)
  (rb--set-flymake-underline-enabled (not rb--flymake-underline-enabled))
  )

(use-package flymake
  :ensure nil
  :config
  (setq python-flymake-command '("ruff" "check" "--quiet" "--stdin-filename=stdin" "-"))
  (setq flymake-mode-line-lighter "Fmake")
  (rb--set-flymake-underline-enabled nil)
  )

(provide 'init-flymake)

;;; init-flymake.el ends here
