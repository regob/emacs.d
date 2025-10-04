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
  (bind-key "w" 'woman 'rb-help-keymap)
  )

;; Read manpages with man
(use-package man
  :ensure nil
  :config
  (bind-key "m" 'man 'rb-help-keymap)
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
  (with-eval-after-load "info"
    (keymap-set Info-mode-map "C-c t" #'casual-info-tmenu))
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
  (setq gptel-model 'gpt-4.1-nano)
  (setq gptel-default-mode 'markdown-mode)
  (bind-key (kbd "C-c RET") 'gptel-send global-map)
  (bind-key (kbd "g") 'gptel-menu rb-help-keymap)
  (bind-key (kbd "c") 'gptel rb-help-keymap)
  (add-to-list 'gptel-directives '(verify . "You are a large language model living in Emacs and a helpful assistant.
Please check the following text for semantical errors (ignore notation,
as the text might be org-mode markup language), and if you see mistakes
make a of the mistakes with brief explanations."))
  )


(defun rb/gptel-verify ()
  "Ask an LLM to check selected text for errors."
  (interactive)
  (let* ((buffer-name "*gptel-out*")
         (rb-buf (get-buffer-create buffer-name)))
    (with-current-buffer rb-buf
      (setq-local buffer-read-only nil)
      (erase-buffer))
    (gptel-request nil
      :callback (lambda (result info) (if (stringp result) (progn (with-current-buffer rb-buf
                                                                    (insert result)
                                                                    (help-mode)
                                                                    (setq-local buffer-read-only t)
                                                                    (display-buffer rb-buf)))))
      :stream nil
      :system "You are a large language model living in Emacs and a helpful assistant.
Please check the following knowledge base snippet for errors, and if you see mistakes make
a list of the mistakes with brief explanations and suggestion for improvement. Only focus on the correctness of the content and IGNORE grammatical and punctuational errors, or tags like :CARD:, since they are part of the markup language (org)! Make sure to output only a single list of issues and their explanations!")
    )
  nil)

(bind-key (kbd "v") 'rb/gptel-verify rb-help-keymap)


(provide 'init-help)

;;; init-help.el ends here
