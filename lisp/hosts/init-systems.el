;;; init-systems.el --- System-specific configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Check what system/OS is being run on and initialize system specific
;; settings
;;; Code:


(defvar is-linux (eq system-type 'gnu/linux))
(defvar is-windows (eq system-type 'windows-nt))
(defvar is-wsl  (and
                 (eq system-type 'gnu/linux)
                 (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop")))


(when is-wsl
  (progn
    (setq browse-url-browser-function #'browse-url-generic)
    (setq browse-url-generic-program "wslview")
    (setq epa-pinentry-mode 'ask)
    )
  )


(provide 'init-systems)

;;; init-systems.el ends here
