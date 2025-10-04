;;; init-javascript.el --- Javascript support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package js
  :ensure nil)

(when-treesit
 (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
 )


(provide 'init-javascript)

;;; init-javascript.el ends here
