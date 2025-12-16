;; nerd_icons.el - nerd font icons and associated packages
;; Part of Knavemacs
;;
;; Includes:
;; - nerd-icons
;; - nerd-icons-completions
;; - nerd-icons-dired
;; - nerd-icons-ibuffer
;; - nerd-icons-corfu
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after vertico marginalia nerd-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure t
  :init (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :init (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))
