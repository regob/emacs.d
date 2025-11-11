;;; init-gptel.el --- GPTel for LLM interactions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when-home
    (defun rb/gptel-verify ()
      "Ask an LLM to check selected text for errors."
      (interactive)
      (let* ((buffer-name "*gptel-out*")
             (rb-buf (get-buffer-create buffer-name)))
        (with-current-buffer rb-buf
          (setq-local buffer-read-only nil)
          (erase-buffer))
        (gptel-request nil
          :callback (lambda (result info) (if (stringp result) (progn (with-current-buffer rb-buf
                                                                        (insert result)
                                                                        (help-mode)
                                                                        (setq-local buffer-read-only t)
                                                                        (display-buffer rb-buf)))))
          :stream nil
          :system "You are a large language model living in Emacs and a helpful assistant.
Please check the following text for errors, and if you see mistakes make
a list of the mistakes with brief explanations. Make sure to output only a single list of issues and their explanations, and IGNORE typographical or grammatical errors!")
        )
      nil)

  (bind-key (kbd "v") 'rb/gptel-verify rb-help-keymap)

  (use-package gptel
    :ensure t
    :pin melpa
    :config
    (setq gptel-api-key (lambda () (getenv "OPENAI_API_KEY")))
    (setq gptel-model 'gpt-4.1-nano)
    (setq gptel-default-mode 'markdown-mode)
    (bind-key (kbd "C-c RET") 'gptel-send global-map)
    (bind-key (kbd "g") 'gptel-menu rb-help-keymap)
    (bind-key (kbd "c") 'gptel rb-help-keymap)
    (add-to-list 'gptel-directives '(verify . "You are a large language model living in Emacs and a helpful assistant.
Please check the following text for semantical errors (ignore notation,
as the text might be org-mode markup language), and if you see mistakes
make a of the mistakes with brief explanations."))
    ))




(provide 'init-gptel)

;;; init-gptel.el ends here
