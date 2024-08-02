;;; init-lisp.el --- Init emacs lisp and common lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; aggressive indent goes well with lisp
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)


(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (defun override-slime-del-key ()
    (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  )

(use-package highlight-quoted
  :init
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
  )

(provide 'init-lisp)

;;; init-lisp.el ends here
