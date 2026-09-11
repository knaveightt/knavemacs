;; git.el - Git-related packages
;; Part of Knavemacs
;;
;; Includes:
;; - diff-hl
;; - magit
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package magit
  :if (eq system-type 'gnu/linux)
  :ensure t)

