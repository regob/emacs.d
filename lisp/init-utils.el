;;; init-utils.el --- General utilities not belonging to elsewhere -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; dependency of dap-mode and treemacs
(use-package hydra
  :ensure (:source "MELPA")
  )

;; ----------------------------------------------------------------------------
;; Calendar
;; ----------------------------------------------------------------------------

(use-package calendar
  :ensure nil
  :init
  ;; show calendar week numbers
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute 'calendar-iso-week-face nil
                      :height 0.7)

  (setq calendar-week-start-day 1
        calendar-intermonth-text
        '(propertize
          (format "%2d"
                  (car
                   (calendar-iso-from-absolute
                    (calendar-absolute-from-gregorian (list month day year)))))
          'font-lock-face 'calendar-iso-week-face))
  )

;; ----------------------------------------------------------------------------
;; Calculator mode
;; ----------------------------------------------------------------------------

(use-package calc
  :ensure nil
  )

(use-package calc-alg
  :ensure nil)

;; ----------------------------------------------------------------------------
;; Writeroom-mode (similar to "zen-mode" in other editors)
;; ----------------------------------------------------------------------------


(use-package writeroom-mode
  :ensure (:source "MELPA")
  :custom
  (writeroom-width 120)
  )

;; ----------------------------------------------------------------------------
;; Jumping in buffer (avy)
;; ----------------------------------------------------------------------------

(use-package avy
  :ensure (:source "MELPA")
  :config
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer)
  )

;; ----------------------------------------------------------------------------
;; Searching
;; ----------------------------------------------------------------------------

;; Rg works best for me (deadgrep is another option)
(use-package rg
  :ensure (:source "MELPA")
  :config
  (rg-enable-menu)
  )

;; ----------------------------------------------------------------------------
;; Misc user functions
;; ----------------------------------------------------------------------------


(defun rb/jump-to-init-file ()
  "Open init.el in the current window."
  (interactive)
  (find-file user-init-file)
  )

(defun rb/byte-recompile ()
  "Byte recompile all init files."
  (interactive)
  (byte-recompile-file user-init-file 0)
  (byte-recompile-directory (expand-file-name "lisp" user-emacs-directory) 0)
  )

(defun rb/search-google (query)
  "Run google search for QUERY in the browser."
  (interactive "sQuery: ")
  (let* ((url (concat "https://google.com/search?q=" (url-hexify-string query))))
    (browse-url-firefox url)
    )
  )

(defun rb/copy-buffer-filename ()
  "Kill the file name opened in currnet buffer or directory (in Dired)."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (kill-new (file-truename default-directory))
    (kill-new (file-truename buffer-file-name))))


(defun rb/open-file-link-at-point ()
  "Opens a file link like 'file.py:line:col' at point."
  (interactive)
  (let* ((str (thing-at-point 'sentence))
         (parts (split-string str ":"))
         (file (car parts))
         (line (cadr parts))
         (col (caddr parts))
         (found-file nil))
    ;; Check if file exists, and try to expand it under project if not
    (cond
     ((null file)
      (message "No file path found at point."))
     ((file-exists-p file)
      (setq found-file file))
     ((project-current) ; Check if we are in a project
      (let* ((project-root (nth 2 (project-current)))
             (potential-file-path (expand-file-name file project-root)))
        (if (file-exists-p potential-file-path)
            (setq found-file potential-file-path)
          (message "File '%s' not found directly or relative to project '%s'."
                   file project-root))))
     (t
      (message "File '%s' not found." original-file)))
    ;; Move to the file/line/column
    (when found-file
      (find-file found-file)
      (when line
        (goto-line (string-to-number line)))
      (when col
        ;; Move to column, if needed. (1- because columns are 0-indexed in Emacs Lisp)
        (goto-char (+ (point-at-bol) (1- (string-to-number col))))))))

(bind-key (kbd "i") #'rb/jump-to-init-file 'rb-user-keymap)
(bind-key (kbd "c") #'rb/byte-recompile 'rb-user-keymap)
(bind-key (kbd "u") #'browse-url-at-point 'rb-user-keymap)
(bind-key (kbd "g") #'rb/search-google 'rb-user-keymap)
(bind-key (kbd "s") #'scratch-buffer 'rb-user-keymap)
(bind-key (kbd "p") #'rb/copy-buffer-filename 'rb-user-keymap)
(bind-key (kbd "f") #'rb/open-file-link-at-point 'rb-user-keymap)


(provide 'init-utils)

;;; init-utils.el ends here
