;; ==================================================
;;; SECTION 5 Elisp-Built Functionality
;; ==================================================

;; -----------------------
;;; 5.1 rainbow delimiters
;; -----------------------

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

;; ----------------
;;; 5.2 acey window
;; ----------------

;; This is built off of emacs-solo by LionyxML
(defvar knavemacs/acey-window-quick-window-overlays nil
  "List of overlays used to temporarily display window labels.")

(defun knavemacs/quick-window-jump ()
  "If there are only two windows, jump to the other. Otherwise, initiate acey window jumping"
  (interactive)
  (if (= (length (window-list)) 2)
      (call-interactively 'other-window)
    (knavemacs/acey-window-quick-window-jump)))

(defun knavemacs/acey-window-quick-window-jump ()
  "Jump to a window by typing its assigned character label.
  Windows are labeled starting from the top-left window and proceeding top to bottom, then left to right."
  (interactive)
  (let* ((window-list (knavemacs/acey-window-get-windows))
         (window-keys (seq-take '("1" "2" "3" "4" "5" "6" "7" "8")
                                (length window-list)))
         (window-map (cl-pairlis window-keys window-list)))
    (knavemacs/acey-window-add-window-key-overlays window-map)
    (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
      (knavemacs/acey-window-remove-window-key-overlays)
      (if-let* ((selected-window (cdr (assoc (char-to-string key) window-map))))
          (select-window selected-window)
        (message "No window assigned to key: %c" key)))))

(defun knavemacs/acey-window-get-windows ()
  "Return a list of windows in the current frame, ordered from top to bottom, left to right."
  (sort (window-list nil 'no-mini)
        (lambda (w1 w2)
          (let ((edges1 (window-edges w1))
                (edges2 (window-edges w2)))
            (or (< (car edges1) (car edges2)) ; Compare top edges
                (and (= (car edges1) (car edges2)) ; If equal, compare left edges
                     (< (cadr edges1) (cadr edges2))))))))

(defun knavemacs/acey-window-add-window-key-overlays (window-map)
  "Add temporary overlays to windows with their assigned key labels from WINDOW-MAP."
  (setq knavemacs/acey-window-quick-window-overlays nil)
  (dolist (entry window-map)
    (let* ((key (car entry))
           (window (cdr entry))
           (start (window-start window))
           (overlay (make-overlay start start (window-buffer window))))
      (overlay-put overlay 'after-string
                   (propertize (format " [%s] " key)
                               'face '(:foreground "#c3e88d"
                                                   :background "#232635"
                                                   :weight bold
                                                   :height default)))
      (overlay-put overlay 'window window)
      (push overlay knavemacs/acey-window-quick-window-overlays))))

(defun knavemacs/acey-window-remove-window-key-overlays ()
  "Remove all temporary overlays used to display key labels in windows."
  (mapc 'delete-overlay knavemacs/acey-window-quick-window-overlays)
  (setq knavemacs/acey-window-quick-window-overlays nil))

;; -----------------------
;;; 5.3 highlight keywords
;; -----------------------

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
