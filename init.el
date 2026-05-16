;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c C-c") #'imenu); eval: (setq imenu-generic-expression '(("Sections" "^;;; \\(.*\\)$" 1))); -*-

;; ==================================================
;;; SECTION 1 Startup Configuration
;; ==================================================

;; Load Behavior Flags
(load (expand-file-name "platform/flags/bflags.el" user-emacs-directory))

;; initial startup speed hack and frame handling
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))
(setq inhibit-compacting-font-caches t)

;; disable UI and startup elements
(menu-bar-mode 1) ; I'm weird, I like seeing this in terminal mode
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t
      use-dialog-box nil
      use-file-dialog nil
      ring-bell-function 'ignore)

;; error reporting levels
(setq warning-minimum-level :error
      warning-suppress-types '((lexical-binding)))

;; ==================================================
;;; SECTION 2 Auxiliary File Configuration
;; ==================================================

;; backup file handling
(setq create-lockfiles nil
      make-backup-files nil
      backup-inhibited t)

;; custom file handling
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))
(load custom-file t)

;; recents file handling
(setq recentf-max-saved-items 300
      recentf-max-menu-items 15
      recentf-auto-cleanup (if (daemonp) 300 'never)
      recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))
(recentf-mode 1)

;; saveplace file handling
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-limit 600)
(save-place-mode 1)

;; savehist file handling
(setq savehist-file (expand-file-name "savehist" user-emacs-directory)
      savehist-save-minibuffer-history t
      savehist-additional-variables '(kill-ring
				      register-alist
				      mark-ring global-mark-ring
				      search-ring regezp-search-ring)
      history-length 300)
(savehist-mode 1)

;; ==================================================
;;; SECTION 3 General Emacs Configuration
;; ==================================================

;; how search works
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil
      search-whitespace-regexp ".*?")
(setq xref-search-program 'ripgrep
      grep-command "rg -nS --no-heading"
      grep-find-ignored-directories
               '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

;; how undo works
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))
(setq kill-do-not-save-duplicates t)

;; how tabs work
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil) ; spaces only
(setq tab-width 4)

;; additional mode switches
(electric-pair-mode 1)
(global-eldoc-mode -1)

;; ==================================================
;;; SECTION 4 Global Keybind Modifications
;; ==================================================
;(define-key dired-mode-map (kbd "C-<return>") 'knavemacs/window-dired-open-directory)
;(define-key dired-mode-map (kbd "C-k") 'kill-current-buffer)
;(define-key dired-mode-map (kbd "C-o") 'knavemacs/dired-open-display-direction)
;(define-key dired-mode-map (kbd "C-i") 'dired-kill-subdir)
;(global-set-key (kbd "M-o") #'knavemacs/quick-window-jump)
;(global-set-key (kbd "M-p") #'knavemacs/window-dired-vc-root-left)
(global-set-key (kbd "S-TAB") #'completion-at-point)
(global-set-key (kbd "S-<iso-lefttab>") #'completion-at-point)
(global-set-key (kbd "M-g r") #'recentf)
(global-set-key (kbd "M-s g") #'grep)
(global-set-key (kbd "C-x ;") #'comment-line)
(global-set-key (kbd "C-c d") #'global-eldoc-mode)
(global-set-key (kbd "C-c m") #'menu-bar-open)
(global-set-key (kbd "RET") #'newline-and-indent)

;; ==================================================
;;; SECTION 5 Use-Package Configuration and Setup
;; ==================================================
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
;;; SECTION 6 Auto-Load External Configuration Files
;; ==================================================

;; function to load .el files in a specific directory
(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (file (directory-files directory))
    (when (string-match "[A-Za-z0-9_-]+\\.el$" file)
      (load (expand-file-name file directory)))))

;; load "base" configurations
(load-directory (expand-file-name "knavemacs/" user-emacs-directory))

;; load mode-specific configurations
(load-directory (expand-file-name "knavemacs/modes/" user-emacs-directory))

;; load external "core" packages
(load-directory (expand-file-name "external/core/" user-emacs-directory))

;; load external "programming" packages
(load-directory (expand-file-name "external/programming/" user-emacs-directory))

;; load external "visual" packages
(load-directory (expand-file-name "external/visual/" user-emacs-directory))

;; load external "programming" packages
;(load-directory (expand-file-name "knavemacs_programming/" user-emacs-directory))

;; load external "visual" packages
;(load-directory (expand-file-name "knavemacs_visual/" user-emacs-directory))

;; load external modeline module (internally built)
;(load-directory (expand-file-name "knavemacs_modeline/" user-emacs-directory))

;; ==================================================
;;; SECTION 7 Platform-Specific Configuration
;; ==================================================

;; clean up and notify
;(if (eq system-type 'gnu/linux) (shell-command "notify-send 'Emacs Configuration Loaded'"))

;; platform specific load files
;(setq platform-files (expand-file-name "platform" user-emacs-directory))
;(add-to-list 'load-path platform-files)
;(require 'knavemacs-platform)

