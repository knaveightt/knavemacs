;; multistate_modal.el - Knavemacs Modal Editing System
;; Part of Knavemacs

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

(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"))

(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package visual-regexp
  :ensure t)

(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package surround
  :ensure t)

(use-package multistate
  :ensure t
  :init
  ;; Emacs state
  (multistate-define-state 'emacs :lighter "E")
  ;; Motion state
  (multistate-define-state
   'motion
   :default t
   :lighter "M"
   :cursor 'hollow
   :parent 'multistate-emacs-state-map)
  ;; Insert state
  (multistate-define-state
   'insert
   :lighter "I"
   :cursor 'bar
   :parent 'multistate-emacs-state-map)
  ;; Normal state
  (multistate-define-state
   'normal
   :lighter "N"
   :cursor 'box
   :parent 'multistate-suppress-map)
  ;; Enable multistate-mode globally
  (multistate-global-mode 1)
  :config
  (defun knavemacs/multistate-move-top-window ()
    "Move point to the top of the window"
    (interactive)
    (move-to-window-line-top-bottom 0))
  (defun knavemacs/multistate-move-bottom-window ()
    "Move point to the top of the window"
    (interactive)
    (move-to-window-line-top-bottom -1))
  (defun knavemacs/multistate-find-file ()
    "Runs find file or set-fill-column depending on if multistate-normal is active"
    (interactive)
    (if (multistate-normal-state-p)
        (call-interactively 'find-file)
      (call-interactively 'set-fill-column)))
  (defun knavemacs/multistate-insert-at-indent ()
    "Moves point to beginning of indentation and enters insert state"
    (interactive)
    (back-to-indentation)
    (multistate-insert-state))
  (defun knavemacs/multistate-insert-at-end-of-line ()
    "Moves point to end of line and enters insert state"
    (interactive)
    (end-of-line)
    (multistate-insert-state))
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
  (defun knavemacs/modal--dwim-delete ()
    "Kills a region if a region is active, otherwise executes kill-line"
    (interactive)
    (if (region-active-p)
	(call-interactively 'kill-region)
      (kill-line)))
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
  (defun knavemacs/modal--jump-to-char ()
    "Read a character from the user and move point to the next occurrence of that character in the buffer."
    (interactive)
    (let* ((target-char (read-char-from-minibuffer "Jump to char: "))
           (char-as-string (char-to-string target-char)))
      (if (search-forward char-as-string nil t)
          (message "Jumped to '%c'" target-char)
        (message "Character '%c' not found after point" target-char))))
  (defun knavemacs/modal--jump-back-to-mark ()
    "Interactive function that attempts to move the cursor to the previously set mark."
    (interactive)
    (setq current-prefix-arg '(4)) ; C-u
    (call-interactively 'set-mark-command))
  (defun knavemacs/multistate-dwim-insert-state ()
    "Go to insert state after executing a delete dwim action."
    (interactive)
    (knavemacs/modal--dwim-delete)
    (multistate-insert-state))
  (defun knavemacs/modal--open-line-below ()
    "Creates a new line below the current line."
    (interactive)
    (end-of-line)
    (electric-newline-and-maybe-indent)
    (multistate-insert-state))
  (defun knavemacs/modal--open-line-above ()
    "Creates a new line above the current line."
    (interactive)
    (beginning-of-line)
    (newline)
    (forward-line -1)
    (multistate-insert-state))
  (defun knavemacs/multistate-replace-region ()
    "Replaces selected region with first item in kill-ring"
    (interactive)
    (delete-region)
    (yank))
  (defun knavemacs/modal--read-replacement-text ()
    "Asks the user for text in the minibuffer to replace the current region."
    (interactive)
    (setq replacement-text (read-from-minibuffer "Replace With: "))
    (call-interactively 'backward-delete-char-untabify)
    (insert replacement-text)
    (kill-new replacement-text))
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
  (defun knavemacs/multistate-select-word ()
    "Selects a word on the current screen based on the first letter."
    (interactive)
    (call-interactively 'avy-goto-word-1)
    (call-interactively 'set-mark-command)
    (forward-word))
  (defun knavemacs/multistate-select-line ()
    "Selects a word on the current screen based on the first letter."
    (interactive)
    (avy-goto-line)
    (knavemacs/modal--set-mark-line)
    (backward-char))
  (defun knavemacs/modal--scroll-down-half-page ()
    "scroll down half a page while keeping the cursor centered" 
    ;; https://www.reddit.com/r/emacs/comments/r7l3ar/how_do_you_scroll_half_a_page/
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
  (defun knavemacs/forward-or-backward-sexp (&optional arg)
    "Go to the matching parenthesis character if one is adjacent to point."
    (interactive "^p")
    (cond ((looking-at "\\s(") (forward-sexp arg))
          ((looking-back "\\s)" 1) (backward-sexp arg))
          ;; Now, try to succeed from inside of a bracket
          ((looking-at "\\s)") (forward-char) (backward-sexp arg))
          ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

  ;; mapping of existing keymaps (mostly to SPC menu)
  ;; along with changes to make this efficient
  (define-key multistate-normal-state-map (kbd "SPC x") ctl-x-map)
  (define-key multistate-normal-state-map (kbd "SPC v") vc-prefix-map)
  (define-key multistate-normal-state-map (kbd "SPC h") help-map)
  (define-key multistate-normal-state-map (kbd "SPC p") project-prefix-map)
  (define-key multistate-normal-state-map (kbd "'") surround-keymap)
  (define-key ctl-x-map (kbd "s") #'(lambda () (interactive) (if (multistate-normal-state-p) (save-buffer) (save-some-buffers))))
  (define-key ctl-x-map (kbd "f") #'knavemacs/multistate-find-file) ;; needs to be called interactively
  (define-key ctl-x-map (kbd "c") #'save-buffers-kill-terminal)
  (define-key ctl-x-map (kbd "j") #'dired-jump)

  ;; custom keymaps using SPC as a leader (normal state)
  (define-key multistate-normal-state-map (kbd "SPC o c") #'org-capture)
  (define-key multistate-normal-state-map (kbd "SPC o a") #'org-agenda)
  (define-key multistate-normal-state-map (kbd "SPC o t") #'knavemacs/org-quick-time-stamp-inactive)
  (define-key multistate-normal-state-map (kbd "SPC o l") #'org-store-link)
  (define-key multistate-normal-state-map (kbd "SPC t t") #'tab-line-mode)
  (define-key multistate-normal-state-map (kbd "SPC t T") #'tab-bar-mode)
  (define-key multistate-normal-state-map (kbd "SPC t j") #'knavemacs/tab-line-pinned-switch-to-buffer)
  (define-key multistate-normal-state-map (kbd "SPC t r") #'knavemacs/tab-line-pinned-reset-buffers)
  (define-key multistate-normal-state-map (kbd "SPC t p") #'knavemacs/tab-line-pinned-pin-buffer)
  (define-key multistate-normal-state-map (kbd "SPC t u") #'knavemacs/tab-line-pinned-unpin-buffer)
  
  ;; custom g keymap
  (define-key multistate-normal-state-map (kbd "g v") #'knavemacs/modal--jump-back-to-mark)
  (define-key multistate-normal-state-map (kbd "g u") #'universal-argument)
  (define-key multistate-normal-state-map (kbd "g f") #'fzf-grep-with-narrowing)
  (define-key multistate-normal-state-map (kbd "g F") #'fzf-grep-in-dir-with-narrowing)

  ;; custom N keymap
  (define-key multistate-normal-state-map (kbd "N q") #'er/mark-inside-quotes)
  (define-key multistate-normal-state-map (kbd "N Q") #'er/mark-outside-quotes)
  (define-key multistate-normal-state-map (kbd "N p") #'er/mark-inside-pairs)
  (define-key multistate-normal-state-map (kbd "N P") #'er/mark-outside-pairs)
  (define-key multistate-normal-state-map (kbd "N d") #'er/mark-defun)
  (define-key multistate-normal-state-map (kbd "N u") #'er/mark-url)
  (define-key multistate-normal-state-map (kbd "N c") #'er/mark-comment)

  ;; custom p keymap
  (define-key multistate-normal-state-map (kbd "p p") #'mc/mark-all-like-this)
  (define-key multistate-normal-state-map (kbd "p s") #'vr/mc-mark)
  (define-key multistate-normal-state-map (kbd "p o") #'mc/mark-pop)

  ;; custom t keymap
  (define-key multistate-normal-state-map (kbd "t u") #'upcase-dwim)
  (define-key multistate-normal-state-map (kbd "t d") #'downcase-dwim)

  ;; custom | keymap
  (define-key multistate-normal-state-map (kbd "| c") #'copy-to-register)
  (define-key multistate-normal-state-map (kbd "| y") #'insert-register)
  (define-key multistate-normal-state-map (kbd "| r") #'kmacro-start-macro)
  (define-key multistate-normal-state-map (kbd "| e") #'kmacro-end-macro)
  
  ;; custom bracket keymaps
  (define-key multistate-normal-state-map (kbd "[ t") #'knavemacs/tab-line-pinned-prev-tab)
  (define-key multistate-normal-state-map (kbd "[ b") #'switch-to-prev-buffer)
  (define-key multistate-normal-state-map (kbd "[ p") #'mc/mark-previous-like-this)
  (define-key multistate-normal-state-map (kbd "] t") #'knavemacs/tab-line-pinned-next-tab)
  (define-key multistate-normal-state-map (kbd "] b") #'switch-to-next-buffer)
  (define-key multistate-normal-state-map (kbd "] p") #'mc/mark-next-like-this)
 
  ;; while motion state is default, however if an editable file is visited
  ;; then enter normal state instead
  (add-hook 'find-file-hook 'multistate-normal-state)
  
  :bind
  (:map multistate-emacs-state-map
        ("C-z" . multistate-normal-state))
  (:map multistate-motion-state-map
        ("C-z" . multistate-emacs-state)
        ("SPC" . multistate-normal-state)
        ("q" . kill-buffer-and-window)
        ("J" . knavemacs/multistate-move-bottom-window)
        ("j" . next-line)
        ("K" . knavemacs/multistate-move-top-window)
        ("k" . previous-line))
  (:map multistate-insert-state-map
        ("C-z" . multistate-emacs-state)
        ("ESC" . multistate-normal-state))
  (:map multistate-normal-state-map
        ("C-z" . multistate-motion-state)
        ("C-j" . windmove-down)
        ("C-k" . windmove-up)
        ("C-h" . windmove-left)
        ("C-l" . windmove-right)
        ("C-b" . switch-to-buffer)
        ("M-o" . knavemacs/quick-window-jump)
        (":" . execute-extended-command)
        ("," . knavemacs/modal--scroll-up-half-page)
        ("." . knavemacs/modal--scroll-down-half-page)
        ("\"" .  surround-insert)
        ("/" . vr/replace)
        ("?" . vr/query-replace)
        ("<" . beginning-of-buffer)
        (">" . end-of-buffer)
        ("%" . knavemacs/forward-or-backward-sexp)
        ("{" . backward-paragraph)
        ("}" . forward-paragraph)
        ("\\" . kmacro-call-macro)
        ("a" . knavemacs/multistate-insert-at-indent)
        ("A" . knavemacs/multistate-insert-at-end-of-line)
        ("b" . backward-word)
        ("B" . backward-sexp)
        ("c" . kill-ring-save)
        ("C" . knavemacs/append-region-to-buffer)
        ("d" . knavemacs/modal--dwim-delete)
        ("D" . kill-whole-line)
        ("e" . knavemacs/modal--end-expansion-forward)
        ("E" . knavemacs/modal--end-expansion-backward)
        ("f" . knavemacs/modal--jump-to-char)
        ("F" . avy-goto-char-timer)
        ; g is a prefix key
        ("G" . knavemacs/tab-line-pinned-prompt-to-jump)
        ("h" . backward-char)
        ("H" . beginning-of-line)
        ("i" . multistate-insert-state)
        ("I" . knavemacs/multistate-dwim-insert-state)
        ("j" . next-line)
        ("J" . knavemacs/multistate-move-bottom-window)
        ("k" . previous-line)
        ("K" . knavemacs/multistate-move-top-window)
        ("l" . forward-char)
        ("L" . end-of-line)
        ("m" . recenter-top-bottom)
        ("M" . move-to-window-line-top-bottom)
        ("n" . er/expand-region)
        ; N is a prefix key
        ("o" . knavemacs/modal--open-line-below)
        ("O" . knavemacs/modal--open-line-above)
        ; p is a prefix key
        ("P" . mc/edit-lines)
        ("q" . kill-current-buffer)
        ("Q" . revert-buffer)
        ("r" . knavemacs/modal--read-replacement-text)
        ("R" . knavemacs/multistate-replace-region)
        ("s" . knavemacs/multistate-select-word)
        ("S" . knavemacs/multistate-select-line)
        ; t is a prefix key
        ("T" . transpose-lines)
        ("u" . undo)
        ("U" . vundo)
        ("v" . knavemacs/modal--set-or-cancel-mark)
        ("V" . knavemacs/modal--set-mark-line)
        ("w" . forward-word)
        ("W" . forward-sexp)
        ("x" . delete-char)
        ("X" . backward-delete-char)
        ("y" . yank)
        ("Y" . yank-pop)
        ("z" . zap-up-to-char)
        ("Z" . zap-to-char)))
