;;; init-lsp.el --- Lsp and dap mode for programming modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook
  (((python-mode c-mode c++-mode) . lsp))
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-keymap-prefix "C-c l")
  (require 'lsp-pyright)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-completion-enable t)
  )

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package dap-mode
  :hook
  (((python-mode) . dap-mode))

  :config
  (dap-ui-mode)
  (dap-ui-many-windows-mode)

  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  )


(provide 'init-lsp)

;;; init-lsp.el ends here
