;;; Emacs init file

; use-package is not needed at runtime
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/"))
; (byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

; Set autosave directory
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;(setq initial-buffer-choice "~")
(setq initial-frame-alist '((top . 0) (left . 0) (width . 120) (height . 80)))


; other window to M-o (default C-x o)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x 4 s") 'forward-symbol)

;; windmove mode for S-<arrow> window navigation
(windmove-default-keybindings)

;; visual line mode enabled, without word wrapping
(setq visual-line-mode t)
(setq word-wrap nil)

;; modes for navigating super_words and CamelCase words
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
;; to update packages: list-packages then S-u x

(use-package better-defaults)

(setq inhibit-startup-message t)    ;; Hide the startup message
(global-linum-mode t)               ;; Enable line numbers globally

;; Enable auto-revert-mode
(global-auto-revert-mode t)

;; indent with spaces
(setq-default indent-tabs-mode  nil)
(setq-default tab-width 4)

;; =========================
;; Appearance configs
;; =========================

; Remove toolbars, menu and scrollbars
;(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; (add-hook 'after-init-hook (lambda() (load-theme 'tango-dark)))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/themes/emacs-theme-gruvbox"))
(require 'gruvbox-theme)
; (use-package 'gruvbox-theme)
(add-hook 'after-init-hook (lambda() (load-theme 'gruvbox)))
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
                                        ; (require 'misc-util)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(use-package centered-cursor-mode
  :init
  (global-centered-cursor-mode)
  ;; (add-hook 'prog-mode-hook #'centered-cursor-mode)
  ;; (add-hook 'text-mode-hook #'centered-cursor-mode)
  )

  
;; =========================
;; Global development setup
;; =========================

;; enable flycheck globally
(use-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-flake8rc "~/.flake8")
  (setq flycheck-pylintrc "~/.pylintrc")
  )
  

(use-package projectile
  :init
  (add-hook 'prog-mode-hook #'(lambda () (projectile-mode +1)))
  :bind-keymap
  ("C-c p" . 'projectile-command-map)
  )

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

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'load-path
             "~/.emacs.d/plugins/yasnippet-radical-snippets")
  (require 'yasnippet-radical-snippets)
  (yasnippet-radical-snippets-initialize)
  )

;; (use-package paredit
;;   :init
;;   (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook 'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;;   (add-hook 'slime-repl-mode-hook 'enable-paredit-mode)
;; )

;; (use-package smartparens
;;   :init
;;   (add-hook 'prog-mode-hook #'smartparens-mode)
;;   (require 'smartparens-config)
;;   :config
;;   )




(use-package multiple-cursors
  :defer nil
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )

;; =========================
;; Python
;; =========================

(use-package python
  :init
  (setq python-shell-interpreter "python3.8")
  (setq python-shell-interpreter-args "-i")
  :commands python-mode
  :interpreter ("python3.8" . python-mode)
  :custom
  (python-environment-virtualenv (quote ("python3.8" "-m" "venv")))
  )


;; use local elpy version for jedi fix
(add-to-list 'load-path "/home/rego/.emacs.d/plugins/elpy")
(require 'elpy)
(advice-add 'python-mode :before 'elpy-enable)
(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
(add-hook 'elpy-mode-hook 'flycheck-mode)
(setq elpy-rpc-python-command "python3.8")
;;  (setq elpy-rpc-backend "company")
(setq elpy-rpc-virtualenv-path 'system)

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

(use-package py-autopep8
  :after (python elpy)
  :bind*
  (:map elpy-mode-map ("C-c C-f" . 'py-autopep8-buffer))
  )


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
;;  (TeX-global-PDF-mode nil)
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

(use-package clang-format
  :init
  (setq clang-format-style "file:~/.styles/clang_format.yaml")
  )

(use-package cc-mode
  :init
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
  (setq c-default-style "linux")
  (setq-default c-basic-offset 4)
  :bind
  (:map c++-mode-map ("C-c C-f" . 'clang-format-buffer))
  )



;; set flycheck standard

;; (define-key c-mode-base-map (kbd "C-c C-f") 'clang-format-buffer)

;; ;; c++ modern syntax highlighting, needed??
;; (require 'modern-cpp-font-lock)
;; (modern-c++-font-lock-global-mode t)


