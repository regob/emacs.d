;;; early-init.el --- Initialization before init.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; do not load site-lisp (packages installed with system package manager)
(setq load-path
      (seq-filter
       (lambda (pth) (not (string-match "^/usr/share/emacs/site-lisp.*" pth)))
       load-path))

(provide 'early-init)

;;; early-init.el ends here
