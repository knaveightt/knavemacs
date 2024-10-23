;; Config.el - part of Knavemacs
(message "[Knavemacs] Determining device profile settings for > %s"
	 knavemacs/device-profile)

;; Default Settings - these are loaded if the knavemacs/device-profile
;; is not recongized as a value that triggers a profile function
(setq knavemacs/config-visual nil)
					; config-visual manages [doom-themes]
					; [whick-key]
					; [vundo]
					; [activities]

(setq knavemacs/config-visual-icons nil)
					; config-visual-icons manages [nerd-icons]
					; [nerd-icons-dired]
					; [nerd-icons-completion]
					; [nerd-icons-ibuffer]

(setq knavemacs/config-fonts nil)
					; config-fonts manages fonts and [ligatures]

(setq knavemacs/config-keybinds nil)
					; config-keybinds manages [meow]
					; [avy]
					; [ace-window]
(setq knavemacs/config-fuzzyfind nil)
					; config-fuzzyfind manages [vertico]
					; [savehist]
					; [marginalia]
					; [orderless]
					; [embark]
					; [consult]
					; [embark-consult]

(setq knavemacs/config-snippets nil)
					; config-snippets manages [yasnippets]

(setq knavemacs/config-programming nil)
					; config-programming manages [magit]

(setq knavemacs/config-dired nil)
					; config-dired manages dired customizations
					; [dired-single]
					; [dired-hide-dotfiles]
					; [dired-subtree]

(setq knavemacs/config-org nil)
					; config-org manages org mode configurations

;; Profile definitions
(defun knavemacs/profile-computer ()
  (message "[Knavemacs] Loading Profile for *Computer*")
  (setq knavemacs/config-visual t)
  (setq knavemacs/config-visual-icons t)
  (setq knavemacs/config-fonts t)
  (setq knavemacs/config-keybinds t)
  (setq knavemacs/config-fuzzyfind t)
  (setq knavemacs/config-snippets t)
  (setq knavemacs/config-programming t)
  (setq knavemacs/config-dired t)
  (setq knavemacs/config-org t))

;; Profile Determination Logic
(if (eq knavemacs/device-profile 'computer)
    (knavemacs/profile-computer))
