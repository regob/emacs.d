;;; init-company.el --- Company mode for completion at point -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; don't use orderless for company capf
;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))

;; (use-package company
;;   ;; :diminish ""
;;   :custom
;;   (company-idle-delay 0.2)
;;   (company-selection-wrap-around t)
;;   (company-minimum-prefix-length 3)
;;   (company-candidates-length 30)
;;   (company-require-match nil)
;;   (company-dabbrev-ignore-case nil)
;;   (company-dabbrev-downcase nil)
;;   (company-show-numbers nil)
  
;;   :config
;;   (bind-keys :map company-active-map
;;              ("TAB" . company-complete))
;;   ;;  (setq company-backends '(company-capf))
;;   (advice-add 'company-capf :around #'company-completion-styles)
;;   (global-company-mode)
;;   )


(provide 'init-company)

;;; init-company.el ends here


