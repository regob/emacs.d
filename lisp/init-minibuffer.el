;;; init-minibuffer.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; vertical completion in the minibuffer
(use-package vertico
  :config
  (vertico-mode)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)
              )

  :init
  (marginalia-mode)
  )


;; Embark - actions on things in minibuffer and at point
(use-package embark
  :config
  (global-set-key (kbd "C-.") 'embark-act))


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
