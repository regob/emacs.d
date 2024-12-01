;;; init-python.el --- Python scripts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package python
  :ensure nil
  :config
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")

  ;; set compile command with current file name
  (add-hook 'python-mode-hook #'(lambda ()
                                  (set (make-local-variable 'compile-command)
                                       (concat "python3 " buffer-file-name))))

  :commands python-mode
  :interpreter ("python3" . python-mode)
  )


(provide 'init-python)

;;; init-python.el ends here
