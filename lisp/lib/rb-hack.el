;;; rb-hack.el --- Utilities for hacking/debugging -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; By @xuchunyang https://emacs.stackexchange.com/a/24658/38136
(defun rb-advice-unadvice (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol: ")
  (advice-mapc (lambda (advice _props) (advice-remove sym advice)) sym))


(defun rb-bind-for-debug ()
  "Prompt for a symbol and bind a behavior to `C-c 5` depending on the symbol type."
  (interactive)
  (let* ((sym (intern (completing-read "Symbol: " obarray)))
         (key (kbd "C-c 5")))
    (cond
     ;; If it's an interactive command
     ((commandp sym)
      (global-set-key key sym)
      (message "Bound command `%s` to %s" sym key))

     ;; If it's a function (but not a command)
     ((fboundp sym)
      (let ((wrapper
             (lambda ()
               (interactive)
               (let ((result (funcall sym)))
                 (message "%s => %s" (symbol-name sym) result)))))
        (fset 'rb--func-wrapper wrapper)
        (global-set-key key 'rb--func-wrapper)
        (message "Bound call to function `%s` with result display to %s" sym key)))

     ;; If it's a variable
     ((symbolp sym)
      (let ((wrapper
             (lambda ()
               (interactive)
               (message "%s => %s" (symbol-name sym) (symbol-value sym)))))
        (fset 'rb--var-wrapper wrapper)
        (global-set-key key 'rb--var-wrapper)
        (message "Bound display of variable `%s` to %s" sym key)
        ))

     ;; Unknown type
     (t (message "Symbol `%s` is not a known command, function, or variable." sym)))))


(provide 'rb-hack)

;;; rb-hack.el ends here
