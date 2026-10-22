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

(defun rb/wl-copy (beg end)
  "Copy region to Wayland clipboard asynchronously."
  (interactive "r")
  (unless (use-region-p)
    (user-error "No active region"))
  (unless (executable-find "wl-copy")
    (user-error "wl-copy not found"))

  (let ((proc (make-process
               :name "wl-copy"
               :buffer nil
               :command '("wl-copy" "--type" "text/plain;charset=utf-8")
               :connection-type 'pipe
               :noquery t)))
    (process-send-string proc
                         (buffer-substring-no-properties beg end))
    (process-send-eof proc))

  (message "Copied to Wayland clipboard"))

(when is-wsl
  (progn
    (setq browse-url-browser-function #'browse-url-generic)
    (setq browse-url-generic-program "wslview")
    (setq epa-pinentry-mode 'loopback)

    ;; needed for C-w and M-w to work for yanking into other programs
    (setq select-active-regions nil)

    (global-set-key (kbd "C-c 9") #'rb/wl-copy)
    )
  )


(provide 'init-systems)

;;; init-systems.el ends here
