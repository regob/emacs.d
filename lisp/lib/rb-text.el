;;; rb-text.el --- Text manipulation functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Some hacky rules to check if a character "belongs" to the previous one,
;; meaning they display as a single glyph.
;; References used:
;; https://www.unicode.org/versions/Unicode15.0.0/ch04.pdf
;; https://www.compart.com/en/unicode/category
(defun rb-char-displayed-with-previous-p (ch)
  "Is CH displayed with the previous character in a single glyph?"
  (or
   ;; a non-zero combining class means it combines with the previous char
   (not (zerop (get-char-code-property ch 'canonical-combining-class)))
   ;; some character categories modify the previous one
   (memq (get-char-code-property ch 'general-category)
         '(Lm                          ; modifier letters
           Mn                          ; Mark, nonspacing
           Mc                          ; Mark, spacing combining
           Sk                          ; Modifier symbol
           )))
  )

;; ̈For example the ZWJ character in this one: 🙋🏾‍♀️
(defun rb-char-unicode-joiner-p (ch)
  "Is CH a ZWJ or equivalent that joins two unicode symbols?"
  (memq ch '(?\u200d)))

(defun rb-reverse-string (str)
  "Reverse STR, paying attention to some multibyte chars and combining classes."
  (let (glyphs
        glyph)
    (dolist (char (string-to-list str))
      (if (rb-char-displayed-with-previous-p char)
          (push char glyph)
        (progn (when glyph (push (nreverse glyph) glyphs))
               (setq glyph (list char))))
      )
    (when glyph (push (nreverse glyph) glyphs))
    ;; some characters might be zero width joiners, we join those with the next glyph
    (let ((joined-glyphs '()))
      (while (not (null glyphs))
        (if (rb-char-unicode-joiner-p (car (car glyphs)))
            ;; if the current char is a joiner, we join the prev and the next glyphs
            (progn (setq joined-glyphs
                         (cons (append (cadr glyphs) (car glyphs) (car joined-glyphs))
                               (cdr joined-glyphs)))
                   (setq glyphs (cddr glyphs))
                   )
          (push (pop glyphs) joined-glyphs))
        )
      ;; the glyphs are in reversed order in joined-glyphs, so we reverse and concat
      (apply 'concat (nreverse joined-glyphs))
      )
    )
  )

(defun rb/reverse-chars-region (start end)
  "Reverse text in region.
START and END are define the region to be reversed."
  (interactive "r")
  (save-excursion
    (let ((new-text (rb-reverse-string (buffer-substring start end))))
      (delete-region start end)
      (goto-char start)
      (insert new-text)
      )))

(provide 'rb-text)

;;; rb-text.el ends here
