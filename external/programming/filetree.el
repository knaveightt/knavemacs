;; ==================================================
;; filetree.el
;;
;; Treemacs incorporation
;;
;; Part of Knavemacs configuration
;;==================================================
(use-package treemacs
  :if knavemacs/BFlags--treemacs
  :ensure t
  :bind ("C-c f" . treemacs)
  :config
  (setq treemacs-project-follow-mode t))

(use-package treemacs-magit
  :if knavemacs/BFlags--treemacs
  :ensure t)

(use-package treemacs-nerd-icons
  :if knavemacs/BFlags--treemacs
  :ensure t
  :config
  (treemacs-nerd-icons-config))

