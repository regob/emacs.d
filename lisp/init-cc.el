;;; init-cc.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package clang-format
;;   :config
;;   (let ((pth (file-name-concat (getenv "HOME")
;;                                ".styles/clang_format.yaml")))
;;     (setq clang-format-style (concat "file:" pth)))
;;   )

(use-package cc-mode
  :ensure nil
  :config
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
  )


;; set flycheck standard

;; (define-key c-mode-base-map (kbd "C-c C-f") 'clang-format-buffer)

;; ;; c++ modern syntax highlighting, needed??
;; (require 'modern-cpp-font-lock)
;; (modern-c++-font-lock-global-mode t)

(provide 'init-cc)

;;; init-cc.el ends here
