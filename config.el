(message "[Knavemacs] Determining device profile settings for > %s"
	 knavemacs/device-profile)

;; Set Defaults - these are loaded if the knavemacs/device-profile variable
;; is not recognized as a value that triggers a profile function below
;;
;; Note: packages in [,] brackets are externally downloaded
;;       packages in (,) parenthesis are internal package configs
(setq knavemacs/config-visual t)        ; [modus-themes]
                                        ; [rainbow-delimiters]
                                        ; [rainbow-mode]
(setq knavemacs/config-visual-icons t)  ; [nerd-icons]
                                        ; [nerd-icons-dired]
                                        ; [nerd-icons-completion]
                                        ; [nerd-icons-ibuffer]
(setq knavemacs/config-compframework t) ; [vertico]
                                        ; [savehist]
                                        ; [marginalia]
                                        ; [orderless]
                                        ; [embark]
                                        ; [consult]
                                        ; [embark-consult]
(setq knavemacs/config-keyhelp t)       ; [Which-key]
(setq knavemacs/config-templates t)     ; [yasnippet]
(setq knavemacs/config-utils t)         ; [vundo]
                                        ; [avy]
                                        ; [ace-window]
(setq knavemacs/config-project t)       ; [magit]
(setq knavemacs/config-programming t)   ; (eglot)
                                        ; [corfu]
                                        ; [kind-icon]
                                        ; [corfu-terminal]
(setq knavemacs/config-dired t)         ; (dired)
                                        ; [dired-single]
                                        ; [dired-hide-dotfiles]
                                        ; [dired-subtree]
(setq knavemacs/config-org t)           ; (org)
                                        ; [org-bullets]
(setq knavemacs/config-notes t)         ; [org-roam]
                                        ; [org-roam-ui]
(setq knavemacs/config-cite t)          ; [ebib]
                                        ; [citar] [citar-embark] [citar-org-roam]
(setq knavemacs/config-keybinds t)      ; [meow]
                                        ; [hydra]
                                        ; [general]
(setq knavemacs/config-linux t)         ; [notifications]

;; Special Notes:
;; 1) modeline configuration :: depends on visual,visual-icons,keybinds

;; functions for specific profiles get added here
;; "mobile" profile
(defun knavemacs/profile-mobile ()
  (message "[Knavemacs] Loading profile for Mobile")
  (setq knavemacs/config-compframework t)  ; t
  (setq knavemacs/config-keyhelp nil)      ; nil
  (setq knavemacs/config-templates nil)    ; nil
  (setq knavemacs/config-dired t)          ; t
  (setq knavemacs/config-org t)            ; t
  (setq knavemacs/config-notes t)          ; t
  (setq knavemacs/config-cite t)           ; t
  (setq knavemacs/config-visual-icons nil) ; nil
  (setq knavemacs/config-visual t)         ; t
  (setq knavemacs/config-utils)            ; t
  (setq knavemacs/config-project t)        ; t
  (setq knavemacs/config-keybinds t)       ; t
  (setq knavemacs/config-linux nil)        ; nil
  )

;; let's determine which profile to use, given all of the above and
;; the value of knavemacs/device-profile
(if (eq knavemacs/device-profile 'mobile) 
  (knavemacs/profile-mobile) 
  )
