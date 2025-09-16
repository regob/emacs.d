;;; init-eglot.el --- Eglot mode for language server interaction -*- lexical-binding: t -*-
;;; Commentary:
;; Experimental setup for eglot
;;; Code:

(use-package eglot
  :ensure nil
  :config
  (bind-key "e" 'eglot 'rb-help-keymap)
  (setcdr
   (assoc
    '(python-mode python-ts-mode)
    eglot-server-programs)
   (eglot-alternatives
    '(("basedpyright-langserver" "--stdio")
      ("ty" "server")
      ("pyright-langserver" "--stdio")
      "pylsp" "pyls" "jedi-language-server" "ruff-lsp")))

  ;; don't spam the echo area
  (setq eglot-report-progress nil)

  ;; configure some basedpyright settings (https://github.com/joaotavora/eglot/issues/1464)
  (setq-default
   eglot-workspace-configuration
   '(:basedpyright
     #1=(:typeCheckingMode
         "recommended"
         :analysis
         #2=(:diagnosticSeverityOverrides
             (:reportUnknownParameterType
              :json-false
              :reportMissingTypeArgument
              :json-false
              :reportMissingParameterType
              :json-false
              :reportUnknownVariableType
              :json-false
              :reportUnknownArgumentType
              :json-false
              :reportUnknownMemberType
              :json-false
              :reportUnknownLambdaType
              :json-false
              :reportAny
              :json-false
              :reportMissingTypeStubs
              :json-false
              )
             :inlayHints
             (:callArgumentNames
              :json-false
              :functionReturnTypes
              t
              :variableTypes
              :json-false
              :genericTypes
              :json-false)))
     :basedpyright.analysis #2#
     ))

  ;; https://www.reddit.com/r/emacs/comments/10yzhmn/flymake_just_works_with_ruff/
  (add-hook 'eglot-managed-mode-hook
            (lambda () (cond ((derived-mode-p 'python-base-mode)
                              (add-hook 'flymake-diagnostic-functions 'python-flymake nil t))
                             ;; if not adding diagnostic functions to other modes just use an if
                             ;; ...
                             (t nil))))

  )

(provide 'init-eglot)
;;; init-eglot.el ends here


