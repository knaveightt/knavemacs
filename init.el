;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c C-c") #'imenu); eval: (setq imenu-generic-expression '(("Sections" "^;;; \\(.*\\)$" 1))); -*-

;; ==================================================
;; Knavemacs
;; Vanilla Emacs Config
;; for Emacs 30.2
;; ==================================================

;; --------------------------------------------------
;;; SECTION 1 Configure the Global Environment
;; --------------------------------------------------
;; UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; initial startup speed hack and frame handling
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))
(setq inhibit-compacting-font-caches t)

;; error reporting levels
(setq warning-minimum-level :error
      warning-suppress-types '((lexical-binding)))

;; --------------------------------------------------
;;; SECTION 2 Auxiliary File Configuration
;; --------------------------------------------------
;; backup file handling
(setq create-lockfiles nil
      make-backup-files nil
      backup-inhibited t)

;; custom file handling
(setq custom-file (expand-file-name "init-custom.el" user-emacs-directory))
(load custom-file t)

;; recents file handling
(setq recentf-save-file (expand-file-name "history/recentf" user-emacs-directory)
      recentf-max-saved-items 300
      recentf-max-menu-items 15
      recentf-auto-cleanup (if (daemonp) 300 'never)
      recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
(recentf-mode 1)

;; saveplace file handling
(setq save-place-file (expand-file-name "history/saveplace" user-emacs-directory)
      save-place-limit 600)
(save-place-mode 1)

;; projects file handling
(setq project-list-file (expand-file-name "history/projects" user-emacs-directory))

;; savehist file handling
(setq savehist-file (expand-file-name "history/savehist" user-emacs-directory)
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring
				      register-alist
				      mark-ring global-mark-ring
				      search-ring regezp-search-ring)
      history-length 300)
(savehist-mode 1)

;; --------------------------------------------------
;;; SECTION 3 Load Internal Configuration Files
;; --------------------------------------------------
;; function to load .el files in a specific directory
(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (file (directory-files directory))
    (when (string-match "[A-Za-z0-9_-]+\\.el$" file)
      (load (expand-file-name file directory)))))

;; Load Behavior Flags
(load-directory (expand-file-name "platform/flags/" user-emacs-directory))

;; Load Startup Configurations (mode-switches, global keybinds, visual init)
(load-directory (expand-file-name "internal/startup/" user-emacs-directory))

;; --------------------------------------------------
;;; SECTION 4 Load External Configuration Files
;; --------------------------------------------------
;; use-package setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
  			 ("org" . "https://orgmode.org/elpa/")
  			 ("elpa" . "https://elpa.gnu.org/packages/")
  			 ))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-hook-name-suffix nil)

;; ==================================================
;;; SECTION 5 Platform-Specific Configuration
;; ==================================================

;; platform specific load files
(setq platform-files (expand-file-name "platform" user-emacs-directory))
(add-to-list 'load-path platform-files)
(require 'knavemacs-platform)

