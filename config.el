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
(setq knavemacs/config-dired t)         ; (dired)
                                        ; [dired-single]
                                        ; [dired-hide-dotfiles]
                                        ; [dired-subtree]
(setq knavemacs/config-org t)           ; (org)
                                        ; [org-bullets]
(setq knavemacs/config-notes t)         ; [org-roam]
                                        ; [org-roam-ui]
(setq knavemacs/config-project t)       ; [projectile]
                                        ; [magit]
(setq knavemacs/config-keybinds t)      ; [meow]
                                        ; [hydra]
                                        ; [general]

;; Special Notes:
;; 1) modeline configuration :: depends on visual,visual-icons,keybinds

;; functions for specific profiles get added here
;; "mobile" profile
(defun knavemacs/profile-mobile ()
  (message "[Knavemacs] Loading profile for Mobile")
  (setq knavemacs/config-compframework t)
  (setq knavemacs/config-keyhelp nil)
  (setq knavemacs/config-dired t)
  (setq knavemacs/config-org t)
  (setq knavemacs/config-notes t)
  (setq knavemacs/config-visual-icons nil)
  (setq knavemacs/config-visual t)
  (setq knavemacs/config-project t)
  (setq knavemacs/config-keybinds t)
  )

;; let's determine which profile to use, given all of the above and
;; the value of knavemacs/device-profile
(if (eq knavemacs/device-profile 'mobile) 
  (knavemacs/profile-mobile) 
  )
