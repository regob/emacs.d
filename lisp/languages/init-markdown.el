;;; init-markdown.el --- Markdown -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Markdown Treesit support seems to be still behind
;; Use normal mode
(use-package markdown-mode
  :ensure t
  :pin nongnu
  :bind (:map
         markdown-mode-map
         ("M-<up>" . markdown-move-up)
         ("M-<down>" . markdown-move-down))
  :config
  (setq markdown-command "pandoc")
  :custom-face
  (markdown-code-face ((t (:inherit org-code))))
  )


(provide 'init-markdown)

;;; init-markup.el ends here
