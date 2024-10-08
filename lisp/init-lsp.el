;;; init-lsp.el --- Lsp and dap mode for programming modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook
  (((python-mode c-mode c++-mode) . lsp))
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-enable t)
  :config
  (require 'lsp-pyright)
  )

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package dap-mode
  :hook
  (((python-mode) . dap-mode))
  :functions (dap-ui-mode
              dap-ui-many-windows-mode)
  :custom
  (dap-python-debugger 'debugpy)
  :config
  (dap-ui-mode)
  (dap-ui-many-windows-mode)
  (require 'dap-python)
  )


(provide 'init-lsp)

;;; init-lsp.el ends here
