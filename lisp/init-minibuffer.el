;;; init-minibuffer.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :init
  (marginalia-mode)
  
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)
              )
  )


;; Use the `orderless' completion style for space separated orderless completion
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles
                                               partial-completion
                                               )))
        )

  )


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
