;;; early-init.el --- Initialization before init.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; disable package.el
(setq package-enable-at-startup nil)
(setq package-archives nil)

;; do not load site-lisp
(setq load-path
      (seq-filter
       (lambda (pth) (not (string-match "/usr/share/emacs/site-lisp.*" pth)))
       load-path))


(provide 'early-init)

;;; early-init.el ends here
