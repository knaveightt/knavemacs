;;; -*- lexical-binding: t; -*-
;; ==================================================
;; geiser.el
;;
;; Scheme support configured to run guile-scheme
;;
;; Part of Knavemacs configuration
;;==================================================
(use-package geiser
  :if knavemacs/BFlags--geiser
  :ensure t
  :defer t
  :custom
  (geiser-active-implementations '(guile))
  (geiser-default-implementation 'guile))

(use-package geiser-guile
  :if knavemacs/BFlags--geiser
  :ensure t
  :after geiser)
