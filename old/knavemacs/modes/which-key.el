(use-package which-key
  :defer t
  :ensure nil
  :hook
  (after-init-hook . which-key-mode)
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 1.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40)

  (which-key-add-key-based-replacements
    "C-c o" "Org Commands"
    ))
