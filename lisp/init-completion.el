;;; init-completion.el --- Completion init (corfu and builtins) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package corfu
  :ensure t
  :pin gnu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-quit-no-match 'separator)
  (corfu-auto-delay 0.3)
  (corfu-left-margin-width 1.0)
  (corfu-right-margin-width 1.0)
  (corfu-bar-width 0.5)
  :init
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)

  :config
  ;; disable popup in eshell mode
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (setq-local completion-styles '(basic))))

  ;; Only useful in elisp, but it's global ... test if breaks anything?
  ;; (corfu-echo-mode)

  ;; corfu seems to be broken in python shell
  ;; returning broken candidates __0_dummy_completion__, inserted by python-mode
  (add-hook 'inferior-python-mode-hook
            (lambda ()
              (corfu-mode -1)
              (setq-local corfu-auto nil)
              (setq-local completion-styles '(basic))))

  ;; python-completion-at-point also calls to shell when its running, which errors
  (add-hook (if-treesit 'python-ts-mode-hook 'python-mode-hook)
            (lambda ()
              (setq-local completion-at-point-functions
                          (remove 'python-completion-at-point completion-at-point-functions))
              (setq-local completion-styles '(basic))))

  ;; Option to move completion to minibuffer when corfu is active
  ;; https://github.com/minad/corfu#transfer-completion-to-the-minibuffer
  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  (keymap-set corfu-map "M-m" #'corfu-move-to-minibuffer)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)

  :custom
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

;; Emacs 30 and newer: Disable Ispell completion function.
;; Try `cape-dict' as an alternative.
(setq text-mode-ispell-word-completion nil)

;; New inline preview in emacs 30
;; https://www.masteringemacs.org/article/whats-new-in-emacs-301
;; (use-package completion-preview
;;   :ensure nil
;;   :hook
;;   ;; conflicts with tab completion
;;   ((inferior-python-mode-hook . completion-preview-mode))
;;   )


(use-package hippie-exp
  :ensure nil
  :config
  (keymap-global-set "M-/" 'hippie-expand)
  )

(use-package yasnippet
  :ensure t
  :pin gnu
  :config
  (yas-global-mode 1)
  (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
  ;; mutable backquote used in org card template
  (require 'warnings)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))


(use-package yasnippet-snippets
  :ensure t
  :pin nongnu)


;; Use the `orderless' completion style for space separated orderless completion
(use-package orderless
  :ensure t
  :pin gnu
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles
                                               partial-completion
                                               )))
        )
  )


(provide 'init-completion)

;;; init-completion.el ends here
