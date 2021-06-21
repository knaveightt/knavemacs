;;; Knavemacs -- Personal Emacs Configuration File
;;;  ___  __    ________   ________  ___      ___ _______   _____ ______   ________  ________  ________
;;; |\  \|\  \ |\   ___  \|\   __  \|\  \    /  /|\  ___ \ |\   _ \  _   \|\   __  \|\   ____\|\   ____\
;;; \ \  \/  /|\ \  \\ \  \ \  \|\  \ \  \  /  / | \   __/|\ \  \\\__\ \  \ \  \|\  \ \  \___|\ \  \___|
;;;  \ \   ___  \ \  \\ \  \ \   __  \ \  \/  / / \ \  \_|/_\ \  \\|__| \  \ \   __  \ \  \    \ \_____  \
;;;   \ \  \\ \  \ \  \\ \  \ \  \ \  \ \    / /   \ \  \_|\ \ \  \    \ \  \ \  \ \  \ \  \____\|____|\  \
;;;    \ \__\\ \__\ \__\\ \__\ \__\ \__\ \__/ /     \ \_______\ \__\    \ \__\ \__\ \__\ \_______\____\_\  \
;;;     \|__| \|__|\|__| \|__|\|__|\|__|\|__|/       \|_______|\|__|     \|__|\|__|\|__|\|_______|\_________\
;;;                                                                                              \|_________|
;;; Commentary:
;;; Inspired by https://www.sandeepnambiar.com/my-minimal-emacs-setup/
;;;
;;; NOTE: For proper installation:
;;; - Have (or set to something else) "Inconsolata Nerd Font" installed
;;; - for treemacs themes and centaur tabs - make sure you run all-the-icons-install-fonts
;;;   to unpack / install the fonts and icons
;;; - if you get a wierd font icon for modified buffer tabs, try installint ttf-hanazono from the package manager
;;; - Make sure .emacs amd .emacs.d/goldspade-small.png are symlinked correctly
;;; - Make sure a ~/.org file is created
;;; - (optional) Install aspell (program) and aspell-en (dictionary) for flyspell/ispell functionality
;;; - I am working on a companion package allowing the launching of pre-defined workspace macros,
;;;   which works by defining the .el file that contains the macro functions.  Defaults to nil.
(defvar knv-workspaces-file nil)
;;; (defvar knv-workspaces-file "/home/knaveightt/Prog/knv-project/knv-workspaces.el")
;;;
;;; NOTE 2: Config file based on my linux setup.  On Windows, I needed to
;;; install transient, magit, and magit-popup from list-packages, and manually
;;; set the git path / diff path like below:
;;; (setq exec-path (append exec-path '("C:/Users/josinski/Desktop/other/cmder/vendor/git-for-windows/bin")))
;;; (setq exec-path (append exec-path '("C:/Users/josinski/AppData/Roaming/.emacs.p")))

;;; Code:
;; Defining of free variables for flycheck
(defvar org-agenda-mode-map)
(defvar help-move-to-line-cycle-in-source)
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
(add-to-list 'default-frame-alist '(font . "Inconsolata Nerd Font 16"))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode +1)
(line-number-mode +1)
(global-display-line-numbers-mode +1)
(column-number-mode t)
(size-indication-mode t)
(setq-default tab-width 4
	      indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(defun my/disable-scroll-bars (frame)
  "FRAME the frame that's created.  Ensure no new frames spawn with a scrollbar."
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)

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
  (load-theme 'doom-tomorrow-night t)
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

(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
        telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1)
  )

;; Configuring Package: all-the-icons
(use-package all-the-icons
  :ensure t)

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

;; Configuring Package: Centaur Tabs
(use-package centaur-tabs
  :ensure t
  :demand
  :config
  (centaur-tabs-mode t)
  (centaur-tabs-headline-match)
  (setq centaur-tabs-style "chamfer")
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-bar 'under)
  (setq x-underline-at-descent-line t)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-set-modified-marker t)
  
  (defun centaur-tabs-buffer-groups ()
      "`centaur-tabs-buffer-groups' control buffers' group rules.

    Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
    All buffer name start with * will group to \"Emacs\".
    Other buffer group by `centaur-tabs-get-group-name' with project name."
      (list
	(cond
	 ((or (string-equal "*" (substring (buffer-name) 0 1))
	      (memq major-mode '(magit-process-mode
				 magit-status-mode
				 magit-diff-mode
				 magit-log-mode
				 magit-file-mode
				 magit-blob-mode
				 magit-blame-mode
				 )))
	  "Emacs")
	 ((derived-mode-p 'prog-mode)
	  "Editing")
	 ((derived-mode-p 'dired-mode)
	  "Dired")
	 ((memq major-mode '(helpful-mode
			     help-mode))
	  "Help")
	 ((memq major-mode '(org-mode
			     org-agenda-clockreport-mode
			     org-src-mode
			     org-agenda-mode
			     org-beamer-mode
			     org-indent-mode
			     org-bullets-mode
			     org-cdlatex-mode
			     org-agenda-log-mode
			     diary-mode))
	  "OrgMode")
	 (t
	  (centaur-tabs-get-group-name (current-buffer))))))
  
  (defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*epc" name)
     (string-prefix-p "*helm" name)
     (string-prefix-p "*Helm" name)
     (string-prefix-p "*Compile-Log*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*company" name)
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*tramp" name)
     (string-prefix-p " *Mini" name)
     (string-prefix-p "*help" name)
     (string-prefix-p "*straight" name)
     (string-prefix-p " *temp" name)
     (string-prefix-p "*Help" name)
     (string-prefix-p "*mybuf" name)
     (string-prefix-p "*dashboard*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
	  (not (file-name-extension name)))
     )))
  :bind
  (:map evil-normal-state-map
        ("g ." . centaur-tabs-forward)
        ("g ," . centaur-tabs-backward)
        ("g y" . centaur-tabs-group-by-projectile-project)
        ("g Y" . centaur-tabs-group-buffer-groups)
        )
  :hook
  (dired-mode . centaur-tabs-local-mode)
  )

;; Configuring Package: minimap
(use-package minimap
  :ensure t
  :bind
  ("<f9>" . minimap-mode)
  :config
  (setq minimap-window-location 'right)
  )

;; Configuring Package: Which Key
(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode +1))

;; Configuring Package: magit / git related items
(use-package transient
  :ensure t
  :demand t)

(use-package magit-popup
  :ensure t
  :demand t)

(use-package magit
  :ensure t
  :bind (("C-M-g" . magit-status)))

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1)
  )

;; Configuring Package: company
(use-package company
  :ensure t
  :diminish company-mode
  :config
  (add-hook 'after-init-hook #'global-company-mode))

;; Configuring Package: flycheck
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; Configuring Package: projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :bind
  (("C-c p f" . helm-projectile-find-file)
   ("C-c p p" . helm-projectile-switch-project)
   ("C-c p s" . projectile-save-project-buffers))
  :config
  (projectile-mode +1)
  )

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

;; Configuring Package: helm-projectile
(use-package helm-projectile
  :ensure t
  :config
  (helm-projectile-on))

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
        '((sequence "BACKLOG(b)" "NEXT(n)" "TODO(t)" "TASK(k)" "WAIT(w)" "HOLD(h)" "GAVE(v)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-todo-keyword-faces
        '(("TODO" . org-warning) ("TASK" . "cyan") ("WAIT" . "yellow") ("HOLD" . "yellow") ("GAVE" . "purple")
          ("BACKLOG". "green") ("NEXT" . "orange") ("DONE" . "blue") ("CANCELED" . "purple")))
  (setq org-agenda-files (list "~/.org"))

  ;; Custom Agenda View
  (setq org-agenda-custom-commands
        '(("d" "Work-Week Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
           (todo "TODO"
                 ((org-agenda-overriding-header "Additional TODOs This Week")))
           (todo "NEXT"
                 ((org-agenda-overriding-header "Activities Planned for Next Week"))))))
        )

  ;; Capture Templates
  (setq org-capture-templates
      '(("m" "Meeting Notes" entry (file+datetree "~/.org/Unfiled.org" "Meetings")
         "* %t %? :unfiled-minutes:")

        ("n" "General Notes" entry (file+datetree "~/.org/Unfiled.org" "General Notes")
         "* %U\n%?" :empty-lines 1)

        ("t" "Task Entry" entry (file+olp "~/.org/Unfiled.org" "Tasks")
         "* TODO %? :unfiled-action:\n  %U\n  %i" :empty-lines 1)
        ))
  )
  
;; Configuring Package: Org Mode Bullets
(use-package org-bullets
  :ensure t
  :hook
  (org-mode . org-bullets-mode)
  )

(use-package toc-org
  :ensure t
  :hook
  (org-mode . toc-org-mode)
  )

;; Configuring Package: Markdown-Mode
(use-package markdown-mode
  :ensure t)

;; Configuring Package: lua-mode
(use-package lua-mode
  :ensure t)

;; Configuring Package: Yaml-Mode
(use-package yaml-mode
  :ensure t)

;; Configuring Package: Page Break Lines
(use-package page-break-lines
  :ensure t)

;; Configuring Package: fontawesome
(use-package fontawesome
  :ensure t)

;; Configuring Package: treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-expand-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-litter-directories            '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :ensure t
  :config
  (treemacs-load-theme "all-the-icons")
  )

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; Configuring Package: Dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "Knaveightt, the system is ready for you.")
  (setq dashboard-startup-banner "~/.emacs.d/goldspade-small.png")
  ;; (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  ;; (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  ;; Format: "(icon title help action face prefix suffix)"
  (setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Github"
         "Browse Github homepage"
         (lambda (&rest _) (browse-url "http://github.com/knaveightt")))
         )))

  (defun dashboard-goto-recent-files ()
    "Go to recent files on dashboard screen."
    (interactive)
    (funcall (local-key-binding "r"))
    )

  (defun dashboard-goto-projects ()
    "Go to projects on dashboard screen."
    (interactive)
    (funcall (local-key-binding "p"))
    )

  (defun dashboard-goto-bookmarks()
    "Go to bookmarks on dashboard screen."
    (interactive)
    (funcall (local-key-binding "m"))
    )

  (defun dashboard-goto-agenda()
    "Go to agenda on dashboard screen."
    (interactive)
    (funcall (local-key-binding "a"))
    )
  
  (with-eval-after-load 'evil
    (evil-define-key 'normal dashboard-mode-map
      "g" 'dashboard-refresh-buffer
      "p" 'dashboard-goto-projects
      "r" 'dashboard-goto-recent-files
      "m" 'dashboard-goto-bookmarks
      "a" 'dashboard-goto-agenda
      )
    )
  )

;; Initiate the Flyspell modes
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(markdown-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; Custom Misc Functions
(defun open-config()
  "Opens .emacs configuration file."
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
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c o") 'open-config)
(global-set-key (kbd "C-c i") 'minimap-mode)
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-l") 'windmove-right)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C--") 'shrink-window)
(global-set-key (kbd "C-=") 'enlarge-window)
(global-set-key (kbd "M-=") 'enlarge-window-horizontally)
(global-set-key (kbd "M--") 'shrink-window-horizontally)
(global-set-key [f6] 'previous-buffer)
(global-set-key [f7] 'next-buffer)
(global-set-key [f9] 'compile)
(global-set-key [f5] 'flyspell-buffer)
(global-set-key (kbd "C-<f5>") 'ispell-buffer)
(global-set-key (kbd "C-SPC")
	(lambda () (interactive "")
	  (switch-to-buffer (other-buffer (current-buffer) t))))
(evil-global-set-key 'normal (kbd "g j") 'scroll-up-command)
(evil-global-set-key 'normal (kbd "g k") 'scroll-down-command)
(evil-global-set-key 'normal (kbd "j") 'evil-next-visual-line)
(evil-global-set-key 'normal (kbd "k") 'evil-previous-visual-line)
(evil-global-set-key 'normal (kbd ", d") 'kill-this-buffer)
(evil-global-set-key 'normal (kbd ", t") 'treemacs)
(evil-global-set-key 'normal (kbd "C-w j") 'split-window-vertically)
(evil-global-set-key 'normal (kbd "C-w l") 'split-window-horizontally)
(evil-global-set-key 'normal (kbd "C-w C-w") 'delete-window)

;; Custom Loading of minimal "workspaces" defined in a different file
(if knv-workspaces-file
    (load knv-workspaces-file)
    )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "bf387180109d222aee6bb089db48ed38403a1e330c9ec69fe1f52460a8936b66" "60940e1f2fa3f4e61e7a7ed9bab9c22676aa25f927d5915c8f0fa3a8bf529821" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default))
 '(package-selected-packages '(use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(org-level-1 ((t (:inherit outline-1 :height 1.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.0))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.0))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  '(org-level-6 ((t (:inherit outline-6 :height 1.0))))
)
(provide '.emacs)
;;; .emacs ends here
