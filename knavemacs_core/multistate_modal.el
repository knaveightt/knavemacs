;; multistate_modal.el - Knavemacs Modal Editing System
;; Part of Knavemacs

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

  ;; mapping of existing keymaps to SPC menu
  ;; along with changes to make this efficient
  (define-key multistate-normal-state-map (kbd "SPC x") ctl-x-map)
  (define-key multistate-normal-state-map (kbd "SPC v") vc-prefix-map)
  (define-key multistate-normal-state-map (kbd "SPC p") project-prefix-map)
  (define-key ctl-x-map (kbd "s") #'(lambda () (interactive) (if (multistate-normal-state-p) (save-buffer) (save-some-buffers))))
  (define-key ctl-x-map (kbd "f") #'knavemacs/multistate-find-file) ;; needs to be called interactively
  (define-key ctl-x-map (kbd "c") #'save-buffers-kill-terminal)
  (define-key ctl-x-map (kbd "j") #'dired-jump)
  
  :bind
  (:map multistate-emacs-state-map
        ("C-z" . multistate-normal-state))
  (:map multistate-motion-state-map
        ("C-z" . multistate-emacs-state)
        ("SPC" . multistate-normal-state)
        ("q" . kill-this-buffer)
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
