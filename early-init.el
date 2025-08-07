;;; early-init.el --- Initialization before init.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; do not load site-lisp
;; (setq load-path
;;       (seq-filter
;;        (lambda (pth) (and
;;                       (not (string-match "/usr/share/emacs/.*site-lisp.*" pth))
;;                       (not (string-match "/usr/share/emacs/.*lisp.*" pth))))
;;        load-path))

;; (add-to-list 'load-path (expand-file-name "lisp/external" user-emacs-directory))



(provide 'early-init)

;;; early-init.el ends here
