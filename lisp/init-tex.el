;;; init-tex.el --- TeX and LaTeX editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; (use-package tex
;;   :ensure
;;   auctex
;;   :config
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;   (auto-fill-mode nil)
;;   (setq LaTeX-verbatim-environments-local (list "lstlisting"))
;;   (define-key TeX-mode-map (kbd "C-c d") 'TeX-doc)
;;   (setq LaTeX-indent-environment-list
;;         '(("verbatim" current-indentation)
;;           ("verbatim*" current-indentation)
;;           ("filecontents" current-indentation)
;;           ("filecontents*" current-indentation)
;;           ("tabular")
;;           ("tabular*")
;;           ("align" LaTeX-indent-tabular)
;;           ("align*" LaTeX-indent-tabular)
;;           ("array" LaTeX-indent-tabular)
;;           ("eqnarray" LaTeX-indent-tabular)
;;           ("eqnarray*" LaTeX-indent-tabular)
;;           ("displaymath")
;;           ("equation")
;;           ("equation*")
;;           ("picture")
;;           ("tabbing")))
;;   )


(provide 'init-tex)

;;; init-tex.el ends here
