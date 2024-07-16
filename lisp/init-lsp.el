;;; init-lsp.el --- Lsp and dap mode for programming modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (((python-mode c-mode c++-mode) . lsp))
  :config
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  )

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package dap-mode
  :init
  (dap-ui-mode)
  (dap-ui-many-windows-mode)

  :hook
  (((python-mode) . dap-mode))

  :config
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy)
  )


(provide 'init-lsp)

;;; init-lsp.el ends here
