;;; init-local.template.el --- Template for customizations in init-local.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun rb-init-misc-local ()
  (setq org-agenda-files (directory-files-recursively "/home/rego/devel/scratches/" "^\\(\\w\\|[-_.]\\)+\\.org$"))
  (push `("d" "diary" entry (file+olp+datetree "diary_file")
          (function (lambda () "")))
        org-capture-templates)
  (setq org-default-notes-file "refile.org")
  )

(add-hook 'elpaca-after-init-hook 'rb-init-misc-local)

(provide 'init-local.template)

;;; init-local.template.el ends here
