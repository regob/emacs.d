;;; init-json.el --- JSON mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package json-ts-mode
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  )


(provide 'init-json)

;;; init-json.el ends here
