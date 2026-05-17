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

(global-set-key (kbd "M-o") #'knavemacs/quick-window-jump)
