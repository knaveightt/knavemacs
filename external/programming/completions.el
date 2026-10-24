;; ==================================================
;; completions.el
;;
;; Full completions support to use with things like
;; LSP and others. Includes icon support as well
;;
;; Part of Knavemacs configuration
;;==================================================
(use-package corfu
  :if knavemacs/BFlags--completions
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1))

(use-package cape
  :if knavemacs/BFlags--completions
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("M-N" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file))
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

(use-package nerd-icons-corfu
  :if (and knavemacs/BFlags--nerd-icons
           knavemacs/BFlags--completions)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package yasnippet-capf
  :if (and knavemacs/BFlags--snippets
           knavemacs/BFlags--completions)
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))
