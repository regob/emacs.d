;;; init-treesit.el --- Tree sitter for language modes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Treesit grammar sources
;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile" "main" "src")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))


(defvar rb-treesit-grammars-enabled
  '(bash dockerfile javascript json python tsx typescript yaml)
  "Language ids pointing to `treesit-language-source-alist` for which treesit grammar is enabled."
  )

(defun rb/install-treesit-grammars ()
  "Install all enabled treesit grammars."
  (interactive)
  (dolist (grammar rb-treesit-grammars-enabled)
    (message "Installing treesit grammar for %s ..." grammar)
    (treesit-install-language-grammar grammar))
  )



(provide 'init-treesit)

;;; init-treesit.el ends here
