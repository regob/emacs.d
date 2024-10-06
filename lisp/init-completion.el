;;; init-completion.el --- Completion init (corfu and builtins) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :custom                               
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match 'separator)
  (corfu-auto-delay 0.3)

  :init
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)
  
  :custom
  ;; disable popup in eshell mode
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))

  ;; Disable indentation+completion using the TAB key.
  (tab-always-indent t)

  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (text-mode-ispell-word-completion nil)

  ;; Emacs 28 and newer: Hide commands in M-x which do not apply to the current
  ;; mode.  Corfu commands are hidden, since they are not used via M-x. This
  ;; setting is useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p)
  )

(use-package hippie-exp
  :ensure nil
  :config
  (keymap-global-set "M-/" 'hippie-expand)
  )

;; Use the `orderless' completion style for space separated orderless completion
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles
                                               partial-completion
                                               )))
        )
  )

;; Extra completion-at-point functions
(use-package cape
  :ensure (:source "MELPA"))


(use-package yasnippet
  :config
  (yas-global-mode 1)
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  )

(use-package yasnippet-snippets)


(provide 'init-completion)

;;; init-completion.el ends here
