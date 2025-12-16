;; completions.el - In-Buffer completions and utilities
;; Part of Knavemacs
;;
;; Includes:
;; - corfu
;; - cape
(use-package corfu
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
