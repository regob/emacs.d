;;; rb-hack.el --- Utilities for hacking/debugging -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; By @xuchunyang https://emacs.stackexchange.com/a/24658/38136
(defun rb-advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))

(provide 'rb-hack)

;;; rb-hack.el ends here
