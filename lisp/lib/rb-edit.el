;;; rb-edit.el --- Editing library functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun rb--region-lines ()
  "Return the start and end positions of the region, adjusted to full lines."
  (let ((start (region-beginning))
        (end (region-end)))
    (when (> start end)
      (setq start (prog1 end (setq end start))))
    (save-excursion
      (goto-char start)
      (setq start (line-beginning-position))
      (goto-char end)
      (unless (eq end (line-beginning-position)) ; avoid including extra line
        (setq end (line-end-position)))
      (list start end))))


(defun rb-grab-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun rb-grab-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))


(provide 'rb-edit)

;;; rb-edit.el ends here
