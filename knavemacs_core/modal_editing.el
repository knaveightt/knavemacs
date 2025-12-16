;; modal_editing.el - Knavemacs Modal Editing System
;; Part of Knavemacs
;;
;; Includes:
;; - expand-region
;; - multiple-cursors
;; - surround
;; - fzf
;; - avy
;; - vundu
;; - ryo-modal <- the core which brings everything together
(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package surround
  :ensure t)

(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"))

(use-package visual-regexp
  :ensure t)

(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package avy
  :ensure t
  :config
  ;; define an avy action to kill a while line based on a selection
  ;; (see https://karthinks.com/software/avy-can-do-anything/)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  ;; add custom avy actions to the action dispatcher
  (setf (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
  	))


(use-package ryo-modal
  :ensure t
  :commands ryo-modal-mode
  :bind
  (("C-z" . ryo-modal-mode)
   ("<escape>" . ryo-modal-mode))
  :config

  ;; --------------------------------------------------
  ;; Begin Modal Editing Custom Functions
  ;; --------------------------------------------------
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

  (defun knavemacs/modal--end-expansion-forward ()
    "Move a region's selection forward a word, or switch to the lower end of a selected region."
    (interactive)
    (if (region-active-p)
        (if (eql (point) (region-end))
            (forward-word)
          (exchange-point-and-mark))
      (call-interactively 'set-mark-command)
      (forward-word)))
         
  (defun knavemacs/modal--end-expansion-backward ()
    "Move a region's selection forward a word, or switch to the lower end of a selected region."
    (interactive)
    (if (region-active-p)
        (if (eql (point) (region-end))
            (exchange-point-and-mark)
          (backward-word))
      (call-interactively 'set-mark-command)
      (backward-word)))

  (defun knavemacs/modal--jump-back-to-mark ()
    "Interactive function that attempts to move the cursor to the previously set mark."
    (interactive)
    (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'set-mark-command))

  (defun knavemacs/modal--jump-to-char ()
    "Read a character from the user and move point to the next occurrence of that character in the buffer."
    (interactive)
    (let* ((target-char (read-char-from-minibuffer "Jump to char: "))
           (char-as-string (char-to-string target-char)))
      (if (search-forward char-as-string nil t)
          (message "Jumped to '%c'" target-char)
        (message "Character '%c' not found after point" target-char))))
  
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
  ;; --------------------------------------------------
  ;; End Modal Editing Custom Functions
  ;; --------------------------------------------------

  ;; special convenience keys for quick actions when entering modal mode
  (ryo-modal-key "C-k" 'kill-current-buffer) ; kill buffers
  (ryo-modal-key "C-M-k" 'kill-buffer-and-window)
  (ryo-modal-key "C-M-j" 'ibuffer) ; list all buffers
  (ryo-modal-key "C-j" 'switch-to-buffer) ; list buffers
  
  ;; mapping of existing keymaps to SPC menu,
  ;; along with changes to those maps to support this
  (define-key ryo-modal-mode-map (kbd "SPC x") ctl-x-map)
  (define-key ryo-modal-mode-map (kbd "SPC v") vc-prefix-map)
  (define-key ryo-modal-mode-map (kbd "SPC p") project-prefix-map)
  (define-key ryo-modal-mode-map (kbd "'") surround-keymap)
  (define-key ctl-x-map (kbd "s") #'(lambda () (interactive) (if ryo-modal-mode (save-buffer) (save-some-buffers))))
  (define-key ctl-x-map (kbd "f") #'knavemacs/modal--find-file) ;; needs to be called interactively
  (define-key ctl-x-map (kbd "c") #'save-buffers-kill-terminal)
  (define-key ctl-x-map (kbd "j") #'dired-jump)

  ;; custom menus using SPC+<key> as leaders
  (ryo-modal-key
   "SPC" '(("t t" tab-line-mode)
	   ("t T" tab-bar-mode)
	   ("t j" knavemacs/tab-line-pinned-switch-to-buffer)
	   ("t r" knavemacs/tab-line-pinned-reset-buffers)
	   ("t p" knavemacs/tab-line-pinned-pin-buffer)
	   ("t u" knavemacs/tab-line-pinned-unpin-buffer)
	   ("r l" list-registers)
	   ("r v" view-register)
	   ("o c" org-capture)
  	   ("o a" org-agenda)
  	   ("o t" knavemacs/org-quick-time-stamp-inactive)
	   ("o l" org-store-link)))

  ;; keyboard modal key mappings
  (ryo-modal-keys
   ("," knavemacs/modal--scroll-up-half-page)
   ("." knavemacs/modal--scroll-down-half-page)
   ("\"" surround-insert)
   ("\\" ryo-modal-repeat)
   ("/" vr/replace)
   ("?" vr/query-replace)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("%" knavemacs/forward-or-backward-sexp)
   (";" kmacro-call-macro)
   (":"
    (("c"
      copy-to-register)
     ("y"
      insert-register)
     ("r"
      kmacro-start-macro)
     ("e"
      kmacro-end-macro)))
   ("+"
    (("j"
      enlarge-window
      :properties ((repeat-map . knavemacs/modal--window-manage-repeat-map)))
     ("k"
      shrink-window
      :properties ((repeat-map . knavemacs/modal--window-manage-repeat-map)))
     ("l"
      enlarge-window-horizontally
      :properties ((repeat-map . knavemacs/modal--window-manage-repeat-map)))
     ("h"
      shrink-window-horizontally
      :properties ((repeat-map . knavemacs/modal--window-manage-repeat-map)))))
   ("["
    (("t"
      knavemacs/tab-line-pinned-prev-tab)))
   ("]"
    (("t"
      knavemacs/tab-line-pinned-next-tab)))
   ("{" backward-paragraph)
   ("}" forward-paragraph)
   ("a" back-to-indentation :exit t)
   ("A" end-of-line :exit t)
   ("b" backward-word)
   ("B" backward-sexp)
   ("c" kill-ring-save) 
   ("C" knavemacs/append-region-to-buffer) 
   ("d" knavemacs/modal--dwim-delete)
   ("D" kill-whole-line)
   ("e" knavemacs/modal--end-expansion-forward)
   ("E" knavemacs/modal--end-expansion-backward)
   ("F" avy-goto-char-timer) 
   ("f" knavemacs/modal--jump-to-char)
   ("g" ; _goto_ commands
    (("v"
      knavemacs/modal--jump-back-to-mark)
     ("u"
      universal-argument)
     ("f"
      fzf-grep-with-narrowing)
     ("F"
      fzf-grep-in-dir-with-narrowing)
     ))
   ("G" knavemacs/tab-line-pinned-prompt-to-jump)
   ("h" backward-char)
   ("H" beginning-of-line)
   ("i" ryo-modal-mode)
   ("I" delete-region :exit t)
   ("j" next-line)
   ("J" knavemacs/modal--shift-point-bottom)
   ("k" previous-line)
   ("K" knavemacs/modal--shift-point-top)
   ("l" forward-char)
   ("L" end-of-line)
   ("M" move-to-window-line-top-bottom)
   ("m" recenter-top-bottom)
   ("n" er/expand-region)
   ("N" ; smart expand region
    (("q"
      er/mark-inside-quotes)
     ("Q"
      er/mark-outside-quotes)
     ("p"
      er/mark-inside-pairs)
     ("P"
      er/mark-outside-pairs)
     ("d"
      er/mark-defun)
     ("u"
      er/mark-url)
     ("c"
      er/mark-comment)))
   ("o" knavemacs/modal--open-line-below :exit t)
   ("O" knavemacs/modal--open-line-above :exit t)
   ("p" ; multiple points
    (("p"
      mc/mark-all-like-this :mc-all t)
     ("s"
      vr/mc-mark :mc-all t)
     ("["
      mc/mark-previous-like-this :mc-all t)
     ("]"
      mc/mark-next-like-this :mc-all t)
     ("o"
      mc/mark-pop)))
   ("P" mc/edit-lines :mc-all t)
   ("Q" revert-buffer)
   ("R" delete-region :then '(yank))
   ("r" knavemacs/modal--read-replacement-text)
   ("s" avy-goto-word-1 :then '(set-mark-command forward-word))
   ("S" avy-goto-line :then '(knavemacs/modal--set-mark-line backward-char))
   ("t" ; transform options
    (("u"
      upcase-dwim)
     ("d"
      downcase-dwim)))
   ("T" transpose-lines)
   ("u" undo)
   ("U" vundo)
   ("v" knavemacs/modal--set-or-cancel-mark)
   ("V" knavemacs/modal--set-mark-line)
   ("w" forward-word) ; forward word
   ("W" forward-sexp) ; backward word
   ("x" delete-char) ; delete character
   ("X" backward-delete-char-untabify) ; reverse delete character (backspace)
   ("y" yank) ; yank
   ("Y" yank-pop) ; yank from kill ring (fuzzy select)
   ("z" zap-up-to-char) ; zap up to char
   ("Z" zap-to-char)) ; zap to char

  (ryo-modal-keys
   ;; First argument to ryo-modal-keys may be a list of keywords.
   ;; These keywords will be applied to all keybindings.
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9"))

  ;; try to enter modal editing when a new file is visited in a buffer
  (add-hook 'find-file-hook 'ryo-modal-mode))


