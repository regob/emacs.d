;;; init-eglot.el --- Eglot mode for language server interaction -*- lexical-binding: t -*-
;;; Commentary:
;; Experimental setup for eglot
;;; Code:
(use-package eglot
  :ensure nil
  :hook
  (((python-mode)
    . eglot-ensure))
  :config
  (setcdr
   (assoc
    '(python-mode python-ts-mode)
    eglot-server-programs)
   (eglot-alternatives
    '(("basedpyright-langserver" "--stdio")
      ("pyright-langserver" "--stdio")
      "pylsp" "pyls" "jedi-language-server" "ruff-lsp")))

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
             (:callArgumentNames :json-false
                                 :functionReturnTypes :json-false
                                 :variableTypes :json-false
                                 :genericTypes :json-false)))
     :basedpyright.analysis #2#
     ))
  )

(provide 'init-eglot)
;;; init-eglot.el ends here


