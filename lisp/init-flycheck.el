;;; init-flycheck.el --- Flycheck config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :init

  ;; Prettify flycheck echo messages and make them integrated into eldoc
  ;; without this, flycheck message overwrites the eldoc help in the echo area
  ;; https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc
  
  (defun rb-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s: %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err))
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun rb-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'rb-flycheck-eldoc nil t)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))

  :hook ((flycheck-mode . rb-flycheck-prefer-eldoc))
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  :custom
  (flycheck-idle-change-delay 2)
  (flycheck-idle-buffer-switch-delay 1)
  )

(provide 'init-flycheck)

;;; init-flycheck.el ends here
