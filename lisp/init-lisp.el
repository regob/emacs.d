;;; init-lisp.el --- Init emacs lisp and common lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package elisp-mode
  :ensure nil
  :config
  (setq sentence-end-double-space nil)
  )

;; Emacs lisp Regression Testing
(use-package ert
  :ensure nil
  :config
  (bind-key "e r" 'ert 'rb-lispy-keymap)
  (bind-key "e d" 'ert-delete-all-tests 'rb-lispy-keymap)
  (bind-key "e k" 'ert-kill-all-test-buffers 'rb-lispy-keymap))

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
  :ensure (:host github :repo "Fanael/highlight-quoted" :branch "master")
  :init
  (add-hook 'emacs-lisp-mode-hook 'highlight-quoted-mode)
  )

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(provide 'init-lisp)

;;; init-lisp.el ends here
