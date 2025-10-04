;;; init-typescript.el --- Typescript support (using treesit) -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(if-treesit
    (use-package typescript-ts-mode
      :ensure nil
      ))


(provide 'init-typescript)

;;; init-typescript.el ends here
