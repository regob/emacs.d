;;; init-keys.el --- Keymaps and general keybindings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ----------------------------------------------------------------------------
;; User keymaps
;; ----------------------------------------------------------------------------


(define-prefix-command 'rb-lispy-keymap)
(global-set-key (kbd "C-c e") 'rb-lispy-keymap)
(bind-key (kbd "r") 'eval-region 'rb-lispy-keymap)
(bind-key (kbd "b") 'eval-buffer 'rb-lispy-keymap)
(bind-key (kbd "t") 'transpose-sexps 'rb-lispy-keymap)

(define-prefix-command 'rb-user-keymap)
(global-set-key (kbd "C-c 8") 'rb-user-keymap)

(define-prefix-command 'rb-help-keymap)
(global-set-key (kbd "C-c 7") 'rb-help-keymap)


;; ----------------------------------------------------------------------------
;; Globals
;; ----------------------------------------------------------------------------

(global-set-key (kbd "M-o") 'other-window)
;; Ctrl+tab similarly as in browsers/vscode
(global-set-key (kbd "C-<tab>") 'next-buffer)
(global-set-key (kbd "C-<iso-lefttab>") 'previous-buffer)


(provide 'init-keys)

;;; init-keys.el ends here
