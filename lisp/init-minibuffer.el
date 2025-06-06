;;; init-minibuffer.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; vertical completion in the minibuffer
(use-package vertico
  :config
  (vertico-mode)
  )

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)
              )

  :init
  (marginalia-mode)
  )


;; Embark - actions on things in minibuffer and at point
(use-package embark
  :config
  (global-set-key (kbd "C-.") 'embark-act))
;; https://emacsredux.com/blog/2025/06/01/let-s-make-keyboard-quit-smarter/
(defun rb-keyboard-quit ()
  "Smater version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
  (interactive)
  (if (active-minibuffer-window)
      (if (minibufferp)
          (minibuffer-keyboard-quit)
        (abort-recursive-edit))
    (keyboard-quit)))
(global-set-key [remap keyboard-quit] #'rb-keyboard-quit)


(provide 'init-minibuffer)

;;; init-minibuffer.el ends here
