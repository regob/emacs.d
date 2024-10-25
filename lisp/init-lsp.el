;;; init-lsp.el --- Lsp and dap mode for programming modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :hook
  (((python-mode c-mode c++-mode) . lsp))
  ;; Fix unfiltered list of completions in corfu 
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2970
  ((lsp-mode . (lambda () (setq completion-category-defaults nil))))
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-completion-enable t)
  (lsp-use-plists t)
  :config
  (keymap-set lsp-mode-map "C-c l" lsp-command-map)
  ;; show lsp keymap names when using which key
  (lsp-enable-which-key-integration t)
  ;; don't start all workspaces automatically
  ;; https://github.com/emacs-lsp/lsp-mode/discussions/3919
  (advice-add 'lsp
              :before
              (lambda (&rest _args)
                (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-delay 0.8)
  :hook
  ((lsp-ui-mode . (lambda () (lsp-ui-sideline-mode -1))))
  )

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

(use-package lsp-pyright
  :ensure (:source "MELPA")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright))))

(provide 'init-lsp)

;;; init-lsp.el ends here
