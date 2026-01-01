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
        ("i" . multistate-insert-state)
        ("j" . next-line)
        ("k" . previous-line)
        ("x" . delete-char)
        ("X" . backward-delete-char)))
