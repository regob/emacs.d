;;; early-init.el --- Initialization before init.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; disable package.el in favor of elpaca
(setq package-enable-at-startup nil)

;; use plists in lsp for performance
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

;; do not load site-lisp
(setq load-path
      (seq-filter
       (lambda (pth) (not (string-match "/usr/share/emacs/site-lisp.*" pth)))
       load-path))


(provide 'early-init)

;;; early-init.el ends here
