;;; init-smartparens.el --- Smartparens mode (paredit alternative) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package smartparens
  :diminish
  :hook
  ((prog-mode . smartparens-mode)
   (inferior-python-mode . smartparens-mode)
   (ielm . smartparens-mode)
   (css-mode . smartparens-mode)
   (org-mode . smartparens-mode)
   (js-json-mode . smartparens-mode)
   (yaml-mode . smartparens-mode)
   (markdown-mode . smartparens-mode))

  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-SPC" . sp-mark-sexp)
   ("M-(" . sp-wrap-round)
   :map rb-lispy-keymap
   ("DEL" . sp-backward-kill-word)
   ("u" . sp-unwrap-sexp)
   ("t" . sp-transpose-sexp)
   ("s" . sp-slurp-hybrid-sexp)
   )
  :config
  (require 'smartparens-config)
  (setq sp-highlight-pair-overlay nil)


  ;; Auto indent after pressing newline in braces like {|}
  ;; https://github.com/Fuco1/smartparens/issues/80
  (defun rb-enter-and-indent-sexp (&rest _ignored)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  (sp-local-pair 'prog-mode "{" nil :post-handlers '((rb-enter-and-indent-sexp "RET")))
  )



(provide 'init-smartparens)
;;; init-smartparens.el ends here
