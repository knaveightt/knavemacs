;; multistate_modal.el - Knavemacs Modal Editing System
;; Part of Knavemacs

(use-package multistate
  :ensure t
  :hook
  ;; enable selection is Visual state
  (multistate-visual-state-enter . (lambda () (set-mark (point))))
  (multistate-visual-state-exit .  deactivate-mark)
  ;; enable overwrite-mode in Replace state
  (multistate-replace-state-enter . overwrite-mode)
  (multistate-replace-state-exit .  (lambda () (overwrite-mode 0)))
  :init
  ;; Emacs state
  (multistate-define-state 'emacs :lighter "E")
  ;; Insert state
  (multistate-define-state
   'insert
   :lighter "I"
   :cursor 'bar
   :parent 'multistate-emacs-state-map)
  ;; Normal state
  (multistate-define-state
   'normal
   :default t
   :lighter "N"
   :cursor 'hollow
   :parent 'multistate-suppress-map)
  ;; Replace state
  (multistate-define-state
   'replace
   :lighter "R"
   :cursor 'hbar)
  ;; Motion state
  (multistate-define-state
   'motion
   :lighter "M"
   :cursor 'hollow
   :parent 'multistate-suppress-map)
  ;; Visual state
  (multistate-define-state
   'visual
   :lighter "V"
   :cursor 'hollow
   :parent 'multistate-motion-state-map)
  ;; Enable multistate-mode globally
  (multistate-global-mode 1)
  :bind
  (:map multistate-emacs-state-map
        ("C-a" . multistate-normal-state))
  (:map multistate-insert-state-map
        ("`" . multistate-normal-state))
  (:map multistate-normal-state-map
        ("C-z" . multistate-emacs-state)
        ("i" . multistate-insert-state)
        ("R" . multistate-replace-state)
        ("v" . multistate-visual-state)
        ("m" . multistate-motion-state)
        ("/" . search-forward)
        ("?" . search-backward)
        ("x" . delete-char)
        ("X" . backward-delete-char))
  (:map multistate-motion-state-map
        ("`" . multistate-normal-state)
        ("h" . backward-char)
        ("j" . next-line)
        ("k" . previous-line)
        ("l" . forward-char)
        ("^" . move-beginning-of-line)
        ("$" . move-end-of-line)
        ("gg" . beginning-of-buffer)
        ("G" . end-of-buffer))
  (:map multistate-replace-state-map
        ("`" . multistate-normal-state)))
