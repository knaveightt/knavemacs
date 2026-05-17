;; This is built off of emacs-solo by LionyxML
(defun knavemacs/rainbow-delimiters ()
  "Apply simple rainbow coloring to parentheses, brackets, and braces in the current buffer.
  Opening and closing delimiters will have matching colors."
  (interactive)
  (let ((colors '(font-lock-keyword-face
                  font-lock-type-face
                  font-lock-function-name-face
                  font-lock-variable-name-face
                  font-lock-constant-face
                  font-lock-builtin-face
                  font-lock-string-face
                  )))
    (font-lock-add-keywords
     nil
     `((,(rx (or "(" ")" "[" "]" "{" "}"))
        (0 (let* ((char (char-after (match-beginning 0)))
                  (depth (save-excursion
                           ;; Move to the correct position based on opening/closing delimiter
                           (if (member char '(?\) ?\] ?\}))
                               (progn
                                 (backward-char) ;; Move to the opening delimiter
                                 (car (syntax-ppss)))
                             (car (syntax-ppss)))))
                  (face (nth (mod depth ,(length colors)) ',colors)))
             (list 'face face)))))))
  (font-lock-flush)
  (font-lock-ensure))
(add-hook 'prog-mode-hook #'knavemacs/rainbow-delimiters)
