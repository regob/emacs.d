;;; init-web.el --- Web modes for HTML/CSS editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package web-mode
  :mode
  (("\\.xml\\'"        . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode))
  )

(use-package css-mode
  :ensure nil
  )


(provide 'init-web)

;;; init-web.el ends here
