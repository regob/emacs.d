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

(use-package woman
  :ensure nil
  :config
  (setq woman-fill-frame t)
  )

(use-package recentf
  :ensure nil
  :config
  (setq recentf-max-saved-items 300)
  (recentf-mode))


(provide 'init-utils)

;;; init-utils.el ends here
