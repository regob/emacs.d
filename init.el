;;; init.el --- Emacs init file  -*- lexical-binding: t; -*-


(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; use-package is not needed at runtime
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/"))
;; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;; Set autosave directory
(setq backup-directory-alist `(("." . "~/.emacs_saves")))
(setq backup-by-copying t)

(setq initial-frame-alist '((top . 0) (left . 0) (width . 120) (height . 80)))

(global-set-key (kbd "C-x 4 s") 'forward-symbol)
;; always ask before killing emacs (does not hold for emacsclient though)
(setq confirm-kill-emacs 'yes-or-no-p)


;; windmove mode for S-<arrow> window navigation
;; (windmove-default-keybindings)

;; visual line mode enabled, without word wrapping
(setq visual-line-mode t)
(setq word-wrap nil)

;; modes for navigating super_words and CamelCase words
(setq global-superword-mode t)
; (setq global-subword-mode t)


;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))
;; to update packages: list-packages then S-u x

(use-package better-defaults)

(setq inhibit-startup-message t)    ; Hide the startup message

;; Enable line numbers only when executing goto-line
;; from http://whattheemacsd.com/
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))

;; Enable auto-revert-mode
(global-auto-revert-mode t)

;; indent with spaces
(setq-default indent-tabs-mode  nil)
(setq-default tab-width 4)

;; =========================
;; Window config
;; =========================

;; Make "C-x o" prompt for a target window when there are more than 2
(use-package switch-window
  :init
  (setq-default switch-window-shortcut-style 'alphabet)
  (setq-default switch-window-timeout nil)
  (global-set-key (kbd "C-x o") 'switch-window)
  (global-set-key (kbd "M-o") 'switch-window)
  )


;; =========================
;; Appearance configs
;; =========================

;; Remove toolbars, menu and scrollbars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; (add-hook 'after-init-hook (lambda() (load-theme 'tango-dark)))
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/themes/emacs-theme-gruvbox"))
(use-package gruvbox-theme
  :ensure t
  :init
  (add-hook 'after-init-hook (lambda() (load-theme 'gruvbox t)))
  )

(set-face-attribute 'default nil
                    :family "SourceCodePro"
                    :height 100)


(use-package minions
  :ensure t
  :init
  (setq minions-mode-line-lighter "☰")
  (minions-mode 1))

;; =========================
;; Misc tools and configs
;; =========================

(setenv "PAGER" "cat")
;; (require 'misc-util)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package centered-cursor-mode
  :init
  ;; (global-centered-cursor-mode)
  ;; (add-hook 'prog-mode-hook #'centered-cursor-mode)
  ;; (add-hook 'text-mode-hook #'centered-cursor-mode)
  )


;; (use-package treemacs)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

;; =========================
;; User functions
;; =========================

(defun rb-jump-to-init-file ()
  (interactive)
  (find-file user-init-file)
  ;; (switch-to-buffer "~/.emacs.d/init.el")
  )

;; my user keymap
(define-prefix-command 'rb-user-keymap)
(global-set-key (kbd "C-c 8") 'rb-user-keymap)
(bind-key (kbd "i") 'rb-jump-to-init-file 'rb-user-keymap)



(use-package anzu
  :init
  (add-hook 'after-init-hook 'global-anzu-mode)
  :config
  (setq anzu-mode-lighter "")
  )

(use-package avy
  :init
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  )

;; =========================
;; Global development setup
;; =========================

;; enable flycheck globally
(use-package flycheck
  :init
  (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(use-package projectile
  :init
  (add-hook 'prog-mode-hook #'(lambda () (projectile-mode +1)))
  :bind-keymap
  ("C-c p" . 'projectile-command-map)
  :custom
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  (add-to-list 'projectile-globally-ignored-directories ".direnv")
  )

;; (use-package ido
;;   :config
;;   (ido-mode))


;; don't use orderless for company capf
;; We follow a suggestion by company maintainer u/hvis:
;; https://www.reddit.com/r/emacs/comments/nichkl/comment/gz1jr3s/
(defun company-completion-styles (capf-fn &rest args)
  (let ((completion-styles '(basic partial-completion)))
    (apply capf-fn args)))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :diminish ""
  :custom
  (company-idle-delay 0.2)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 3)
  (company-candidates-length 30)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-show-numbers nil)
  :config
  (bind-keys :map company-active-map
             ("TAB" . company-complete))
  ;;  (setq company-backends '(company-capf))
  (advice-add 'company-capf :around #'company-completion-styles)
  )



(use-package rainbow-delimiters
  :init
  ;; turn on rainbow delims in all programming languages and LaTeX
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'LaTeX-mode-hook #'rainbow-delimiters-mode)

  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66") ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6") ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f") ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6") ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc") ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c") ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc") ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999") ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666") ; dark gray
  )

(use-package aggressive-indent
  :hook
  ((css-mode . aggressive-indent-mode))
  )

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  ;; (add-to-list 'load-path
  ;;              "~/.emacs.d/plugins/yasnippet-radical-snippets")
  ;; (require 'yasnippet-radical-snippets)
  ;; (yasnippet-radical-snippets-initialize)
  )

(use-package yasnippet-snippets)

(electric-pair-mode -1)
(electric-indent-mode -1) ; TODO: still gets turned on in cc-mode


(use-package smartparens
  :init
  ;; (add-hook 'python-mode-hook #'smartparens-mode)
  ;; (add-hook 'css-mode-hook #'smart
  (require 'smartparens-config)
  :hook
  (;;(python-mode . smartparens-mode)
   (css-mode . smartparens-mode)
   (prog-mode . smartparens-mode))
  )




(use-package multiple-cursors
  :defer nil
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  )

;; version control stuff (git)
(use-package magit)

(use-package diff-hl
  :init
  (add-hook 'after-init-hook #'global-diff-hl-mode)
  ;; :hook
  ;; ((prog-mode . diff-hl-mode)
  ;;  (org-mode . diff-hl-mode))
  )

(use-package git-modes)

(use-package vertico
  :init
  (vertico-mode)
  (add-hook 'minibuffer-mode-hook (lambda ()
                                    (centered-cursor-mode -1)))
  ;; :bind (:map minibuffer-local-map
  ;;             ("C-v" . ccm-scroll-up)
  ;;             )
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)
              )


  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles
                                               partial-completion
                                               regexp)))))




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



;; optionally if you want to use debugger
;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;   :config
;;   (which-key-mode))


;; =========================
;; Web stuff
;; =========================

(use-package web-mode
  :mode
  (("\\.xml\\'"        . web-mode)
   ("\\.html\\'"       . web-mode)
   ("\\.htm\\'"        . web-mode))

  ;; :hook
  ;; (web-mode . web-mode-toggle-current-element-highlight)
  )

(use-package css-mode
  :ensure nil
  )

;; =========================
;; shell
;; =========================

(use-package sh-script
  :ensure nil
  :config
  (setq sh-basic-offset 4)
  )



;; =========================
;; Python
;; =========================

(use-package python
  :init
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (add-hook 'python-mode-hook #'electric-indent-local-mode)

  ;; set compile command with current file name
  (add-hook 'python-mode-hook #'(lambda ()
                                  (set (make-local-variable 'compile-command)
                                       (concat "python3 " buffer-file-name))))

  :commands python-mode
  :interpreter ("python3" . python-mode)
  ;;(python-environment-virtualenv (quote ("python3.8" "-m" "venv")))
  )


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright))))

(use-package envrc
  :init
  (add-hook 'after-init-hook 'envrc-global-mode)
  )


;; use local elpy version for jedi fix
;; (add-to-list 'load-path "/home/rego/.emacs.d/plugins/elpy")
;; (use-package 'elpy)
;; (advice-add 'python-mode :before 'elpy-enable)
;; (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;; (add-hook 'elpy-mode-hook 'flycheck-mode)
;; (setq elpy-rpc-python-command "python3.8")
;; ;;  (setq elpy-rpc-backend "company")
;; (setq elpy-rpc-virtualenv-path 'system)

;; (use-package elpy
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   :after (python flycheck)
;;   :config
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode)
;;   (setq elpy-rpc-python-command "python3.8")
;; ;;  (setq elpy-rpc-backend "company")
;;   (setq elpy-rpc-virtualenv-path 'system)
;;   )

;; (use-package py-autopep8
;;   :after (python elpy)
;;   :bind*
;;   (:map elpy-mode-map ("C-c C-f" . 'py-autopep8-buffer))
;;   )


;; jedi provides auto completion for Python programs. Depends on the
;; Python packages "jedi" and "epc" to be installed on the host
;; machine. Don't use this with company, install company-jedi instead
;; (use-package jedi
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (add-hook 'python-mode-hook 'jedi:ac-setup)
;;   (setq jedi:complete-on-dot t)
;;   ;; (setq jedi:server-command
;;   ;;       '("/home/rego/.emacs.d/.python-environments/default/bin/jediepcserver"))
;;   :after python
;;   )


;; =========================
;; R
;; =========================

(use-package ess)


;; =========================
;; Markdown
;; =========================

(use-package markdown-mode)

;; throws error for some X stuff
;; (use-package grip-mode
;;   :config
;;   ;;  (define-key markdown-mode-command-map (kbd "g") #'grip-mode)
;;   (add-hook 'markdown-mode-hook #'grip-mode)
;;   )



;; =========================
;; CSV
;; =========================

(use-package csv-mode)

;; =========================
;; Org mode
;; =========================

(use-package org
  :init
  (setq org-startup-truncated nil)
  :bind
  (:map org-mode-map ("C-c C-x t" . 'org-table-create))
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python t)))
  )

(use-package org-anki
  :after org
  :config
  (customize-set-variable 'org-anki-default-deck "dump")
  )


;; =========================
;; LaTeX
;; =========================

(use-package tex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (auto-fill-mode nil)
  (setq LaTeX-verbatim-environments-local (list "lstlisting"))
  :ensure
  auctex
  :config
  (define-key TeX-mode-map (kbd "C-c d") 'TeX-doc)
  (setq LaTeX-indent-environment-list
        '(("verbatim" current-indentation)
          ("verbatim*" current-indentation)
          ("filecontents" current-indentation)
          ("filecontents*" current-indentation)
          ("tabular")
          ("tabular*")
          ("align" LaTeX-indent-tabular)
          ("align*" LaTeX-indent-tabular)
          ("array" LaTeX-indent-tabular)
          ("eqnarray" LaTeX-indent-tabular)
          ("eqnarray*" LaTeX-indent-tabular)
          ("displaymath")
          ("equation")
          ("equation*")
          ("picture")
          ("tabbing")))
  )

;; =========================
;; Kotlin, Java
;; =========================

(use-package flycheck-kotlin)

(use-package kotlin-mode
  :init
  :after flycheck
  :config
  )


;; =========================
;; Erlang
;; =========================


; (use-package erlang)

;; =========================
;; Prolog
;; =========================

; associate file extension .pl with this mode
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; =========================
;; Haskell
;; =========================

(use-package haskell-mode)

;; =========================
;; Lisp
;; =========================

;;(use-package emacs-lisp)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)


(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (defun override-slime-del-key ()
    (define-key slime-repl-mode-map
                (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  )



;; =========================
;; C / C++ setup
;; =========================

;; (use-package clang-format
;;   :config
;;   (let ((pth (file-name-concat (getenv "HOME")
;;                                ".styles/clang_format.yaml")))
;;     (setq clang-format-style (concat "file:" pth)))
;;   )

(use-package cc-mode
  :init
  (setq c-default-style "linux")
  (setq-default c-basic-offset 4)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard
                                            "c++17")))
  (add-hook 'c-mode-common-hook (lambda () (c-toggle-hungry-state 1)))
  (add-hook 'c-mode-common-hook 'smartparens-mode)
  ;; (add-hook 'c-mode-common-hook #'aggressive-indent-mode)

  ;; :bind
  ;; (:map c++-mode-map ("C-c C-f" . 'clang-format-buffer))
  ;; (:map c-mode-map ("C-c C-f" . 'clang-format-buffer))
  :config
  )


;; set flycheck standard

;; (define-key c-mode-base-map (kbd "C-c C-f") 'clang-format-buffer)

;; ;; c++ modern syntax highlighting, needed??
;; (require 'modern-cpp-font-lock)
;; (modern-c++-font-lock-global-mode t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access nil nil nil "Customized with use-package company")
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(switch-window org-anki envrc inheritenv dap-python dap-mode emacs-lisp yasnippet-snippets web-mode lsp-pyright lsp-ui lsp-mode anzu dired-sidebar treemacs fireplace melancholy-theme clang-format kotlin-mode flycheck-kotlin py-autopep8 use-package rainbow-delimiters projectile multiple-cursors gruvbox-theme centered-cursor-mode better-defaults auctex aggressive-indent))
 '(python-indent-trigger-commands '(indent-for-tab-command yas-expand yas/expand newline))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
