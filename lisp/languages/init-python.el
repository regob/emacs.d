;;; init-python.el --- Python scripts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun rb--python-setup ()
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; set compile command with current file name
  (add-hook 'python-mode-hook #'(lambda ()
                                  (set (make-local-variable 'compile-command)
                                       (concat "python3 " buffer-file-name))))

  )

(if-treesit
 (use-package python
   :ensure nil
   :commands python-ts-mode
   :interpreter ("python3" . python-ts-mode)
   :mode ("\\.py\\'" . python-ts-mode)
   :config
   (rb--python-setup)
   )
 (use-package python
   :ensure nil
   :commands python-mode
   :interpreter ("python3" . python-mode)
   :mode ("\\.py\\'" . python-mode)
   :config
   (rb--python-setup)
   )
 )

(use-package cython-mode
  :ensure nil
  )


(provide 'init-python)

;;; init-python.el ends here
