;; filetree.el - File Tree Navigation capability
;; Part of Knavemacs
;;
;; Includes:
;; - treemacs
;; - treemacs-magit
;; - treemacs-nerd-icons
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-project-follow-mode t))

(use-package treemacs-magit
  :ensure t)

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-nerd-icons-config))

