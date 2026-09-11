;; vterm.el - terminal emulator configuration
;; Part of Knavemacs
(use-package vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("C-c C-t" . vterm-copy-mode)) ; Switch to copy mode easily
  :config
  (setq vterm-max-scrollback 10000))
