;; Config.el - part of Knavemacs
(message "[Knavemacs] Determining device profile settings for > %s"
	 knavemacs/device-profile)

;; Default Settings - these are loaded if the knavemacs/device-profile
;; is not recongized as a value that triggers a profile function
(setq knavemacs/config-visual nil)
;; [Colorscheme]
;; ef-themes
;; rainbow-delimiters
;; rainbow-mode
;; dashboard

(setq knavemacs/config-fonts nil)
;; [Font Configuration]
;; (default-frame-alist)
;; nerd-icons
;; nerd-icons-dired
;; nerd-icons-completion
;; nerd-icons-ibuffer
;; [Mode Line Configuration]
;; (mode-line-format) {1/2}

(setq knavemacs/config-utility nil)
;; [Key Indicators]
;; which-key
;; [Fuzzy Finding Completions]
;; vertico
;; savehist
;; marginalia
;; orderless
;; embark
;; consult
;; embark-consult
;; vundo
;; activities

(setq knavemacs/config-dired nil)
;; [Dired Configuration]
;; dired-hide-dotfiles
;; dired-subtree

(setq knavemacs/config-productivity nil)
;; [Org Mode]
;; org
;; [Note Taking System]
;; markdown-mode
;; howm

(setq knavemacs/config-programming nil)
;; [elisp-slime-nav]
;; elisp-slime-nav
;; [Snippets]
;; yasnippets
;; [Eglot Language Server]
;; eglot
;; [In-Buffer Completions at Point]
;; corfu
;; kind-icon


(setq knavemacs/config-keybinds nil)
;; [Mode Line Configuration]
;; (mode-line-format) {1/2}
;; [Jump-To-Location]
;; avy
;; [Window Navigation]
;; ace-window
;; [Modal Editing]
;; meow
;; Profile definitions

(defun knavemacs/profile-computer ()
  (message "[Knavemacs] Loading Profile for *Computer*")
  (setq knavemacs/config-visual t)
  (setq knavemacs/config-fonts t)
  (setq knavemacs/config-utility t)
  (setq knavemacs/config-dired t)
  (setq knavemacs/config-productivity t)
  (setq knavemacs/config-programming t)
  (setq knavemacs/config-keybinds t))

;; Profile Determination Logic
(if (eq knavemacs/device-profile 'computer)
    (knavemacs/profile-computer))
