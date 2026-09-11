(setq ibuffer-saved-filter-groups
      '(("default"
         ("org" (or
          	 (mode . org-mode)
          	 (name . "^\\*Org Src")
          	 (name . "^\\*Org Agenda\\*$")))
         ("tramp" (name . "^\\*tramp.*"))
         ("emacs" (or
          	   (name . "^\\*scratch\\*$")
          	   (name . "^\\*Messages\\*$")
          	   (name . "^\\*Warnings\\*$")
          	   (name . "^\\*Shell Command Output\\*$")
          	   (name . "^\\*Async-native-compile-log\\*$")
          	   (name . "^\\*straight-")))
         ("ediff" (or
          	   (name . "^\\*ediff.*")
          	   (name . "^\\*Ediff.*")))
         ("dired" (mode . dired-mode))
         ("terminal" (or
          	      (mode . term-mode)
          	      (mode . shell-mode)
          	      (mode . eshell-mode)))
         ("help" (or
          	  (name . "^\\*Help\\*$")
          	  (name . "^\\*info\\*$")
          	  (name . "^\\*helpful"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups
:init
(set-window-margins (selected-window) 2 0)
