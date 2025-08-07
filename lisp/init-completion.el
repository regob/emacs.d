;;; init-completion.el --- Completion init (corfu and builtins) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package hippie-exp
  :ensure nil
  :config
  (keymap-global-set "M-/" 'hippie-expand)
  )


(provide 'init-completion)

;;; init-completion.el ends here
