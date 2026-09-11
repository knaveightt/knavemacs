;; This is built off of emacs-solo by LionyxML
(defface knavemacs/HL-hack
  '((t :foreground "#221111" :background "#ff4411" :weight bold))
  "Face for HACK tags."
  :group 'knavemacs/highlight-faces)

(defface knavemacs/HL-todo
  '((t :foreground "#AAA" :background "#2233FF" :weight bold))
  "Face for TODO tags."
  :group 'knavemacs/highlight-faces)

(defface knavemacs/HL-fixme
  '((t :foreground "#221111" :background "#F9E900" :weight bold))
  "Face for FIXME tags."
  :group 'knavemacs/highlight-faces)

(defface knavemacs/HL-note
  '((t :foreground "#221111" :background "#22CC33" :weight bold))
  "Face for NOTE tags."
  :group 'knavemacs/highlight-faces)

(defcustom +highlight-keywords-faces
  '(("TODO" . knavemacs/HL-todo)
    ("FIXME" . knavemacs/HL-fixme)
    ("HACK" . knavemacs/HL-hack)
    ("NOTE" . knavemacs/HL-note))
  "Alist of keywords to highlight and their face."
  :group '+highlight-keywords
  :type '(alist :key-type (string :tag "Keyword")
                :value-type (symbol :tag "Face"))
  :set (lambda (sym val)
         (dolist (face (mapcar #'cdr val))
           (unless (facep face)
             (error "Invalid face: %s" face)))
         (set-default sym val)))

(defvar +highlight-keywords--keywords
  (when +highlight-keywords-faces
    (let ((keywords (mapcar #'car +highlight-keywords-faces)))
      `((,(regexp-opt keywords 'words)
         (0 (when (nth 8 (syntax-ppss))
              (cdr (assoc (match-string 0) +highlight-keywords-faces)))
            prepend)))))
  "Keywords and corresponding faces for `knavemacs/highlight-keywords-mode'.")

(defun knavemacs/highlight-keywords-mode-on ()
  (font-lock-add-keywords nil +highlight-keywords--keywords t)
  (font-lock-flush))

(defun knavemacs/highlight-keywords-mode-off ()
  (font-lock-remove-keywords nil +highlight-keywords--keywords)
  (font-lock-flush))

(define-minor-mode knavemacs/highlight-keywords-mode
  "Highlight TODO and similar keywords in comments and strings."
  :lighter " +HL"
  :group '+highlight-keywords
  (if knavemacs/highlight-keywords-mode
      (knavemacs/highlight-keywords-mode-on)
    (knavemacs/highlight-keywords-mode-off)))

(defun knavemacs/highlight-keywords-hook ()
  "Function that runs on a hook to highlight keywords after a moment."
  (run-at-time "1 sec" nil #'knavemacs/highlight-keywords-mode-on))

(add-hook 'prog-mode-hook #'knavemacs/highlight-keywords-hook)
