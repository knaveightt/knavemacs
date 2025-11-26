(defvar knavemacs/modal--window-manage-repeat-map
  (let ((map (make-sparse-keymap)))
	(define-key map (kbd "j") 'enlarge-window)
	(define-key map (kbd "k") 'shrink-window)
	(define-key map (kbd "l") 'enlarge-window-horizontally)
	(define-key map (kbd "h") 'shrink-window-horizontally)
	map))

(put 'enlarge-window 'repeat-map 'knavemacs/modal--window-manage-repeat-map)
(put 'shrink-window 'repeat-map 'knavemacs/modal--window-manage-repeat-map)
(put 'enlarge-window-horizontally 'repeat-map 'knavemacs/modal--window-manage-repeat-map)
(put 'shrink-window-horizontally 'repeat-map 'knavemacs/modal--window-manage-repeat-map)

;; https://www.reddit.com/r/emacs/comments/r7l3ar/how_do_you_scroll_half_a_page/
(defun knavemacs/modal--scroll-down-half-page ()
  "scroll down half a page while keeping the cursor centered" 
  (interactive)
  (let ((ln (line-number-at-pos (point)))
        (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) (move-to-window-line nil))
          ((= ln lmax) (recenter (window-end)))
          (t (progn
               (move-to-window-line -1)
               (recenter))))))

(defun knavemacs/modal--scroll-up-half-page ()
  "scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
        (lmax (line-number-at-pos (point-max))))
    (cond ((= ln 1) nil)
          ((= ln lmax) (move-to-window-line nil))
          (t (progn
               (move-to-window-line 0)
               (recenter))))))

(defun knavemacs/modal--find-file ()
  "Runs find file or set-fill-column depending on ryo-modal-mode"
  (interactive)
  (if ryo-modal-mode
      (call-interactively 'find-file)
    (call-interactively 'set-fill-column)))

(defun knavemacs/forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(defun knavemacs/modal--dwim-delete ()
  "Kills a region if a region is active, otherwise executes kill-line"
  (interactive)
  (if (region-active-p)
	  (call-interactively 'kill-region)
	(kill-line)))

(defun knavemacs/modal--increment-expression ()
  "Increment the region forward a symbolic expression."
  (interactive)
  (if (not (region-active-p))
	  (progn
		(call-interactively 'set-mark-command)
		(forward-sexp))
	(progn
	  (forward-sexp))))

(defun knavemacs/modal--decrement-expression ()
  "Decrement the region forward a symbolic expression."
  (interactive)
  (if (not (region-active-p))
	  (progn
		(call-interactively 'set-mark-command)
		(backward-sexp))
	(progn
	  (backward-sexp))))

(defun knavemacs/modal--jump-back-to-mark ()
  "Interactive function that attempts to move the cursor to the previously set mark."
  (interactive)
  (setq current-prefix-arg '(4)) ; C-u
  (call-interactively 'set-mark-command))

(defun knavemacs/modal--shift-point-bottom ()
  "Move the point to the bottom of the window without any scrolling."
  (interactive)
  (let ((current-prefix-arg '-))
    (call-interactively 'move-to-window-line-top-bottom)))

(defun knavemacs/modal--shift-point-top ()
  "Move the point to the top of the window without any scrolling."
  (interactive)
  (move-to-window-line-top-bottom '(0)))

(defun knavemacs/modal--open-line-below ()
  "Creates a new line below the current line."
  (interactive)
  (end-of-line)
  (electric-newline-and-maybe-indent))

(defun knavemacs/modal--open-line-above ()
  "Creates a new line above the current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1))

(defun knavemacs/modal--set-or-cancel-mark ()
  "Marks the current point location, or cancels an active region."
  (interactive)
  (if (not (region-active-p))
      (call-interactively 'set-mark-command)
    (keyboard-quit)))

(defun knavemacs/modal--set-mark-line ()
  "Selects the current line."
  (interactive)
  (if (not (region-active-p))
      (progn 
	(beginning-of-line)
	(call-interactively 'set-mark-command)
	(forward-line))
    (forward-line)))

(defun knavemacs/modal--read-replacement-text ()
  "Asks the user for text in the minibuffer to replace the current region."
  (interactive)
  (setq replacement-text (read-from-minibuffer "Replace With: "))
  (call-interactively 'backward-delete-char-untabify)
  (insert replacement-text)
  (kill-new replacement-text))

(defun knavemacs/append-region-to-buffer (target-buffer start end)
  "Prompt for a buffer, then copy the region from the current buffer to the
kill ring, switch to the chosen buffer, append the region's content there,
and finally switch back to the original buffer.

This function does not actually use the kill ring for the transfer internally
(for robustness), but it accomplishes the user's workflow request.

TARGET-BUFFER is the name or buffer object to append to.
START and END define the region in the source buffer."
  (interactive "BAppend region to buffer: \nr")
  (let* ((original-buffer (current-buffer))
         (region-text (buffer-substring-no-properties start end)))
    (unless (region-active-p)
      (error "A region must be active (marked) to use this command"))

    ;; Save to kill ring as requested, without using it for the main logic
    (kill-new region-text)

    (with-current-buffer (get-buffer-create target-buffer)
      (unless buffer-read-only
	(goto-char (point-max))
	(electric-newline-and-maybe-indent)
	(insert region-text)
	(electric-newline-and-maybe-indent)))
      
    ;; Switch back to the original buffer
    (switch-to-buffer original-buffer)

    (message "Region appended to buffer '%s' and returned to original buffer." (buffer-name (get-buffer target-buffer)))))

(provide 'knavemacs-modal-keyfuncs)
