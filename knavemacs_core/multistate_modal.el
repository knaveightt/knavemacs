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


  ;; mapping of existing keymaps to SPC menu
  ;; along with changes to make this efficient
  (define-key multistate-normal-state-map (kbd "SPC x") ctl-x-map)
  (define-key multistate-normal-state-map (kbd "SPC v") vc-prefix-map)
  (define-key multistate-normal-state-map (kbd "SPC p") project-prefix-map)
  (define-key ctl-x-map (kbd "s") #'(lambda () (interactive) (if (multistate-normal-state-p) (save-buffer) (save-some-buffers))))
  (define-key ctl-x-map (kbd "f") #'knavemacs/multistate-find-file) ;; needs to be called interactively
  (define-key ctl-x-map (kbd "c") #'save-buffers-kill-terminal)
  (define-key ctl-x-map (kbd "j") #'dired-jump)

  ;; custom keymaps using SPC as a leader (normal state)
  (define-key multistate-normal-state-map (kbd "SPC o c") #'org-capture)
  (define-key multistate-normal-state-map (kbd "SPC o a") #'org-agenda)
  (define-key multistate-normal-state-map (kbd "SPC o t") #'knavemacs/org-quick-time-stamp-inactive)
  (define-key multistate-normal-state-map (kbd "SPC o l") #'org-store-link)
  
  ;; custom g keymap
  (define-key multistate-normal-state-map (kbd "g v") #'knavemacs/modal--jump-back-to-mark)
  (define-key multistate-normal-state-map (kbd "g u") #'universal-argument)
  (define-key multistate-normal-state-map (kbd "g f") #'fzf-grep-with-narrowing)
  (define-key multistate-normal-state-map (kbd "g F") #'fzf-grep-in-dir-with-narrowing)
  
  ;; while motion state is default, however if an editable file is visited
  ;; then enter normal state instead
  (add-hook 'find-file-hook 'multistate-normal-state)
  
  :bind
  (:map multistate-emacs-state-map
        ("C-z" . multistate-normal-state))
  (:map multistate-motion-state-map
        ("C-z" . multistate-emacs-state)
        ("SPC" . multistate-normal-state)
        ("q" . kill-current-buffer)
        ("J" . knavemacs/multistate-move-bottom-window)
        ("j" . next-line)
        ("K" . knavemacs/multistate-move-top-window)
        ("k" . previous-line))
  (:map multistate-insert-state-map
        ("C-z" . multistate-emacs-state)
        ("ESC" . multistate-normal-state))
  (:map multistate-normal-state-map
        ("C-z" . multistate-emacs-state)
        ("ESC" . multistate-motion-state)
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
        ("h" . backward-char)
        ("H" . beginning-of-line)
        ("i" . multistate-insert-state)
        ("j" . next-line)
        ("J" . knavemacs/multistate-move-bottom-window)
        ("k" . previous-line)
        ("K" . knavemacs/multistate-move-top-window)
        ("l" . forward-char)
        ("L" . end-of-line)
        ("x" . delete-char)
        ("X" . backward-delete-char)))
