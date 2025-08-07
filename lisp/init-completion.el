;;; init-completion.el --- Completion init (corfu and builtins) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hippie-exp
  :ensure nil
  :config
  (keymap-global-set "M-/" 'hippie-expand)
  )

;; Use the `orderless' completion style for space separated orderless completion
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles
                                               partial-completion
                                               )))
        )
  )


(provide 'init-completion)

;;; init-completion.el ends here
