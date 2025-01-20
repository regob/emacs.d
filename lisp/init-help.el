;;; init-help.el --- Utilities for finding stuff  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Show transient key suggestions when typing a key chord
(use-package which-key
  :diminish which-key-mode
  :ensure t
  :config
  (which-key-mode))


(use-package apropos
  :ensure nil
  :config
  (setq apropos-do-all t))

;; Read manpages without man
(use-package woman
  :ensure nil
  :config
  (setq woman-fill-frame t)
  )

;; Neat documentation features
(use-package eldoc
  :diminish
  :ensure nil
  :config
  ;; (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
  (setq eldoc-echo-area-prefer-doc-buffer t)
  ;; don't display multiple line helps (e.g. from eglot/lsp)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eldoc-idle-delay 0.3)
  ;; add keybind for eldoc
  (bind-key "d" 'eldoc 'rb-lispy-keymap)

  (defvar rb--eldoc-html-patterns
    '(("&nbsp;" " ")
      ("&lt;" "<")
      ("&gt;" ">")
      ("&amp;" "&")
      ("&quot;" "\"")
      ("&apos;" "'"))
    "List of (PATTERN . REPLACEMENT) to replace in eldoc output.")

  (defun rb--string-replace-all (patterns in-string)
    "Replace all cars from PATTERNS in IN-STRING with their pair."
    (mapc (lambda (pattern-pair)
            (setq in-string
                  (string-replace (car pattern-pair) (cadr pattern-pair) in-string)))
          patterns)
    in-string)

  (defun rb--eldoc-preprocess (orig-fun &rest args)
    "Preprocess the docs to be displayed by eldoc to replace HTML escapes."
    (let ((doc (car args)))
      ;; The first argument is a list of (STRING :KEY VALUE ...) entries
      ;; we replace the text in each such string
      ;; see docstring of `eldoc-display-functions'
      (when (listp doc)
        (setq doc (mapcar
                   (lambda (doc) (cons
                                  (rb--string-replace-all rb--eldoc-html-patterns (car doc))
                                  (cdr doc)))
                   doc
                   ))
        )
      (apply orig-fun (cons doc (cdr args)))))

  (advice-add 'eldoc-display-in-buffer :around #'rb--eldoc-preprocess)
  )

;; ----------------------------------------------------------------------------
;; Casual suite - transient interfaces to commands
;; ----------------------------------------------------------------------------

(use-package casual
  :ensure (:source "MELPA")
  :config
  (with-eval-after-load "calc-alg"
    (keymap-set calc-alg-map "C-c t" #'casual-calc-tmenu))
  (with-eval-after-load "calc"
    (keymap-set calc-mode-map "C-c t" #'casual-calc-tmenu))
  (with-eval-after-load "calendar"
    (keymap-set calendar-mode-map "C-c t" #'casual-calendar))
  (with-eval-after-load "dired"
    (keymap-set dired-mode-map "C-c t" #'casual-dired-tmenu))  
  (with-eval-after-load "ibuffer"
    (keymap-set ibuffer-mode-map "C-c t" #'casual-ibuffer-tmenu))
  (with-eval-after-load "re-builder"
    (progn
      (keymap-set reb-mode-map "C-c t" #'casual-re-builder-tmenu)
      (keymap-set reb-lisp-mode-map "C-c t" #'casual-re-builder-tmenu)))  
  )

;; ----------------------------------------------------------------------------
;; Dictionary
;; ----------------------------------------------------------------------------

(use-package dictionary
  :ensure nil
  :custom
  (dictionary-use-single-buffer t)
  :config
  (bind-key (kbd "d") #'dictionary-lookup-definition 'rb-user-keymap))

;; ----------------------------------------------------------------------------
;; GPTel - LLM interface for emacs
;; ----------------------------------------------------------------------------

(use-package gptel
  :ensure (:source "MELPA")
  :config
  (setq gptel-api-key (lambda () (getenv "OPENAI_API_KEY")))
  (setq gptel-model "gpt-4o-mini")
  (setq gptel-default-mode 'markdown-mode)
  )

(provide 'init-help)

;;; init-help.el ends here
