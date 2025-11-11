;;; init-html.el ---  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mhtml-mode
  :ensure nil
  :bind (:map mhtml-mode-map
              ("M-o M-o" . nil)
              ("C-c 8" . nil)
              ("M-o" . nil))
)


(provide 'init-html)

;;; init-html.el ends here
