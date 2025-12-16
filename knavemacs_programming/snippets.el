;; snippets.el - Snippets Packages
;; Part of Knavemacs
;;
;; Includes:
;; - yasnippet
;; - yasnippet-capf
(use-package yasnippet
  :ensure t
  :config
  (setq sdir (concat user-emacs-directory "snippets"))
  (setq yas-snippet-dirs
  	'(sdir))
  (yas-global-mode 1) ;; or M-x yas-reload-all if YASnippet is already started
  )

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))
