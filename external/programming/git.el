;; ==================================================
;; git.el
;;
;; Git / Version Control Related Packages
;;
;; Part of Knavemacs configuration
;;==================================================
(use-package diff-hl
  :if knavemacs/BFlags--git
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package magit
  :if knavemacs/BFlags--git
  :ensure t)
