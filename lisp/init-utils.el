;;; init-utils.el --- General utilities not belonging to elsewhere -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package calendar
  :ensure nil
  :init
  ;; show calendar week numbers
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
                      :height 0.7)

  (setq calendar-week-start-day 1
        calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))
  )


(use-package writeroom-mode
  :init
  (setq writeroom-width 120)
  )

;; =========================
;; Misc user functions
;; =========================

(defun rb-jump-to-init-file ()
  "Open init.el in the current window."
  (interactive)
  (find-file user-init-file)
  )

(defun rb-byte-recompile ()
  "Byte recompile all init files."
  (interactive)
  (byte-recompile-file user-init-file 0)
  (byte-recompile-directory (expand-file-name "lisp" user-emacs-directory) 0)
  )

(define-prefix-command 'rb-user-keymap)
(global-set-key (kbd "C-c 8") 'rb-user-keymap)
(bind-key (kbd "i") 'rb-jump-to-init-file 'rb-user-keymap)
(bind-key (kbd "c") #'rb-byte-recompile 'rb-user-keymap)



(provide 'init-utils)

;;; init-utils.el ends here
