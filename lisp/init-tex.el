;;; init-tex.el --- TeX and LaTeX editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package tex-mode
  :ensure nil
  :config
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  ;; maybe configure LaTeX-indent-environment-list (removed)
  )


(provide 'init-tex)

;;; init-tex.el ends here
