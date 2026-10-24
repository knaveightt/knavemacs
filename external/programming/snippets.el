;; ==================================================
;; snippets.el
;;
;; Snippets inclusion to support programming
;;
;; Part of Knavemacs configuration
;;==================================================
(use-package yasnippet
  :if knavemacs/BFlags--snippets
  :ensure t
  :config
  (setq sdir (concat user-emacs-directory "snippets"))
  (setq yas-snippet-dirs
  	'(sdir))
  (yas-global-mode 1) ;; or M-x yas-reload-all if YASnippet is already started
  )

