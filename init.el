;; Set autosave directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq initial-buffer-choice "~")

; Remove toolbars, menu and scrollbars
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; apperance
(add-hook 'after-init-hook (lambda() (load-theme 'tango-dark)))
(set-face-attribute 'default nil
                    :family "SourceCodePro"
                    :height 100
                    )

;; other window to M-o (default C-x o)
(global-set-key (kbd "M-o") 'other-window)

;; windmove mode for S-<arrow> window navigation
(windmove-default-keybindings)

;; visual line mode enabled, without word wrapping
(setq visual-line-mode t)
(setq word-wrap nil)

;; modes for super_words and CamelCase words
(setq global-superword-mode t)
; (setq global-subword-mode t)

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; init use-package
(require 'use-package)
(setq use-package-always-ensure t)


(use-package better-defaults)
(use-package clang-format)

(setq inhibit-startup-message t)    ;; Hide the startup message
(global-linum-mode t)               ;; Enable line numbers globally
(setq electric-pair-preserve-balance nil)

;; Enable auto-revert-mode
(global-auto-revert-mode t)

;; indent with spaces
(setq-default indent-tabs-mode  nil)

;; =========================
;; Convenience packages
;; =========================

(use-package minions
  :ensure t
  :init
  (setq minions-mode-line-lighter "☰")
  (minions-mode 1)
  )
  

;; =========================
;; Global development setup
;; =========================

;; enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)


;; (require 'company)
;; (define-global-minor-mode my-global-company-mode company-mode
;;   (lambda ()
;;     (when (not (memq major-mode
;;                      (list 'python-mode 'minibuffer-mode)))
;;       (company-mode))))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :diminish ""
  :custom
  (company-idle-delay 0.2)
  (company-selection-wrap-around t)
  (company-minimum-prefix-length 2)
  (company-candidates-length 30)
  (company-require-match nil)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-show-numbers t)
  :config
  (bind-keys :map company-active-map
             ("TAB" . company-complete))
  (setq lsp-completion-provider :capf))


(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray
  )

(use-package paredit
  :init
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
)

;; =========================
;; Python
;; =========================


(use-package python
  :commands python-mode
  :interpreter ("python3" . python-mode)
  :custom
  (python-environment-virtualenv (quote ("python3" "-m" "venv")))
  )
(setq python-shell-interpreter "python3")

(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
;  (setq elpy-rpc-pythonpath "/home/rego/.emacs.d/elpa/elpy-20200202.2031/")
  (setq elpy-rpc-virtualenv-path 'system)

  ;; (add-hook 'elpy-mode-hook
  ;;           (lambda () (local-set-key (kbd "C-TAB") #'company-jedi)))
  )

;; jedi provides auto completion for Python programs. Depends on the
;; Python packages "jedi" and "epc" to be installed on the host
;; machine. Don't use this with company, install company-jedi instead
(use-package jedi
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup)
  (setq jedi:complete-on-dot t)
  (setq jedi:server-command
        '("/home/rego/.emacs.d/.python-environments/default/bin/jediepcserver"))

;  (setq jedi:setup-keys t)
  )

(use-package py-autopep8
  :config
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )

;; =========================
;; LaTeX
;; =========================
(use-package tex
  :init
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  :ensure
  auctex
  
  :config
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
;; Common Lisp
;; =========================

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  (defun override-slime-del-key ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))
  (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
)



;; =========================
;; C / C++ setup
;; =========================


;; ;; clang format keybinding
;; (require 'clang-format)
;; (require 'cc-mode)
;; (define-key c-mode-base-map (kbd "C-c C-f") 'clang-format-buffer)

;; ;; c++ modern syntax highlighting, needed??
;; (require 'modern-cpp-font-lock)
;; (modern-c++-font-lock-global-mode t)

;; ;; set flycheck standard
;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))

;; ;; c-style
;; (setq c-default-style "linux"
;;       c-basic-offset 4)




