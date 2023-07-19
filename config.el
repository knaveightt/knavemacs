(message "[Knavemacs] Determining device profile settings for > %s"
	 knavemacs/device-profile)

;; Set Defaults - these are loaded if the knavemacs/device-profile variable
;; is not recognized as a value that triggers a profile function below
;;
;; Note: packages in [,] brackets are externally downloaded
;;       packages in (,) parenthesis are internal package configs
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
(setq knavemacs/config-notes t)         ; [denote]
                                        ; [markdown-mode]
(setq knavemacs/config-visual-icons t)  ; [all-the-icons]
                                        ; [all-the-icons-dired]
(setq knavemacs/config-visual t)        ; [doom-themes]
                                        ; [doom-modeline]
                                        ; [rainbow-delimiters]
                                        ; [rainbow-mode]
(setq knavemacs/config-project t)       ; [projectile]
                                        ; [magit]
(setq knavemacs/config-keybinds t)      ; [meow]
                                        ; [hydra]
                                        ; [general]

;; functions for specific profiles get added here
;; "mobile" profile
(defun knavemacs/profile-mobile ()
  (message "[Knavemacs] Loading profile for Mobile")
  (setq knavemacs/config-compframework t) ; true
  (setq knavemacs/config-keyhelp nil) ; nil
  (setq knavemacs/config-dired t) ; true
  (setq knavemacs/config-org t) ; true
  (setq knavemacs/config-notes t) ; true
  (setq knavemacs/config-visual-icons nil) ; nil
  (setq knavemacs/config-visual t) ; true
  (setq knavemacs/config-project t) ; true
  (setq knavemacs/config-keybinds t) ; true
  )

;; let's determine which profile to use, given all of the above and
;; the value of knavemacs/device-profile
(if (eq knavemacs/device-profile 'mobile) 
  (knavemacs/profile-mobile) 
  )
