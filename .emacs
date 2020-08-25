;;; Emacs Configuration File
;;; Inspired by https://www.sandeepnambiar.com/my-minimal-emacs-setup/
;;;
;;; NOTE: Config file setup for Linux. If using Windows, you may need to
;;; install transient, magit, and magit-popup from list-packages, and manually
;;; set the git path like below:
;;; (setq exec-path (append exec-path '("C:/Users/josinski/Desktop/other/cmder/vendor/git-for-windows/bin")))
;;;
;;; NOTE 2: For proper installation of neotree themes - make sure you run all-the-icons-install-fonts
;;; to unpack / install the fonts and icons 

;; Who am I - Info
(setq user-full-name "John Osinski"
      user-mail-address "johnosinski80@gmail.com")

;; Garbage Collection and Memory Settings
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; File formatting and encoding settings
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Backup File Management
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Visual Setup
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode 1)
(column-number-mode t)
(size-indication-mode t)
(setq-default tab-width 4
	      indent-tabs-mode nil)
(setq inhibit-startup-screen t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
       (abbreviate-file-name (buffer-file-name))
       "%b"))))

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; Package Manager Setup
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Configuring Package: doom-themes w/ solaire
(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode +1))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-acario-dark t)
  (doom-themes-visual-bell-config))

;; Configuring Package: Diminish (hides minor modes)
(use-package diminish
  :ensure t)

;; Configuring Package: Smart Parens
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;; Configuring Package: SmartModeLine (w/ Themes)
(use-package smart-mode-line-powerline-theme
  :ensure t)

(use-package smart-mode-line-atom-one-dark-theme
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  ;;(setq sml/theme 'powerline) ; different theme options
  (setq sml/theme 'atom-one-dark)
  (add-hook 'after-init-hook 'sml/setup))

;; Configuring Package: Neotree
(use-package all-the-icons
  :ensure t)

(use-package neotree
  :ensure t
  :bind
  ("<f8>" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; Configuring Package: Evil
(use-package evil
  :ensure t
  :config
  (setq evil-normal-state-tag   (propertize " NORMAL " 'face '((:background "dark khaki" :foreground "black")))
        evil-emacs-state-tag    (propertize "  EMACS  " 'face '((:background "turquoise" :foreground "black")))
        evil-insert-state-tag   (propertize " INSERT  " 'face '((:background "dark sea green" :foreground "black")))
        evil-replace-state-tag  (propertize " REPLACE " 'face '((:background "dark orange" :foreground "black")))
        evil-motion-state-tag   (propertize " MOTION  " 'face '((:background "khaki" :foreground "black")))
        evil-visual-state-tag   (propertize " VISUAL  " 'face '((:background "light salmon" :foreground "black")))
        evil-operator-state-tag (propertize " OPERATE " 'face '((:background "sandy brown" :foreground "black"))))
  (evil-mode 1))

;; Configuring Package: Which Key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; Configuring Package: magit
(use-package transient
  :ensure t
  :demand t)

(use-package magit-popup
  :ensure t
  :demand t)

(use-package magit
  :ensure t
  :bind (("C-M-g" . magit-status)))

;; Configuring Package: Helm
(use-package helm
  :ensure t
  :defer 2
  :bind
  ("M-x" . helm-M-x)
  ("C-x C-f" . helm-find-files)
  ("M-y" . helm-show-kill-ring)
  ("C-x b" . helm-mini)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (setq helm-split-window-inside-p t
        help-move-to-line-cycle-in-source t)
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 20)
  (helm-autoresize-mode 1)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)
  )

;; Configuring Package: Org
(use-package org
  :ensure t
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :config
  (setq org-log-done t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "FUTURE(f)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning) ("WAITING" . "yellow")
          ("FUTURE". "green") ("DONE" . "blue") ("CANCELED" . "purple")))
  (setq org-agenda-files (list "~/.org"))
  (setq org-default-notes-file "~/.org/captured.org"))

;; Custom Misc Functions
(defun open-config()
  "Opens .emacs configuration file"
  (interactive)
  (find-file "~/.emacs"))

(defun kill-all-dired-buffers ()
      "Kill all dired buffers."
      (interactive)
      (save-excursion
        (let ((count 0))
          (dolist (buffer (buffer-list))
            (set-buffer buffer)
            (when (equal major-mode 'dired-mode)
              (setq count (1+ count))
              (kill-buffer buffer)))
          (message "Killed %i dired buffer(s)." count))))

(defun kill-other-buffers ()
      "Kill all other buffers."
      (interactive)
      (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; Custom Key Mappings
(define-key global-map "\C-co" 'open-config)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "C-+") 'enlarge-window)
(global-set-key (kbd "C-=") 'shrink-window)
(global-set-key (kbd "M-+") 'enlarge-window-horizontally)
(global-set-key (kbd "M-=") 'shrink-window-horizontally)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default))
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
