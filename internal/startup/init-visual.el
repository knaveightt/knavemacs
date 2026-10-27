;; ==================================================
;; init-visual.el
;; Visual Initialization - configurations that define
;; the base look-and-feel of this emacs configuration
;;
;; Part of Knavemacs configuration
;;==================================================

;; Disable/Enable starting UI elements
(menu-bar-mode -1) ; I'm weird, I like seeing this in terminal mode
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t
      use-dialog-box nil
      use-file-dialog nil
      ring-bell-function 'ignore)

;; font configuration when in graphical mode
(add-to-list 'default-frame-alist
	     '(font . "GeistMono NF 12"))

;; starting theme config
(load-theme 'modus-vivendi-tinted t)

;; further theme configuration
(set-display-table-slot standard-display-table 'vertical-border ?\u2502) ; vert separator on terminal
(set-display-table-slot standard-display-table 'truncation ?\u2192) ; character showing truncation on terminal

;; ------------------------------
;; line and line number behavior
;; ------------------------------
;; line wrapping rules
(set-default 'truncate-lines t)

;; line numbers activation
(setq display-line-numbers-type t)
(global-display-line-numbers-mode t)

;; what modes to not show line numbers
(defun knavemacs/no-line-nums-hook ()
  "Supress showing line numbers for select modes."
  (display-line-numbers-mode 0))

(dolist (mode '(term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		dired-mode-hook))
  (add-hook mode 'knavemacs/no-line-nums-hook))

;; ----------------------------
;; line scroll behavior
;; ----------------------------
(setq scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t)

;; ----------------------------
;; window and split behavior
;; ----------------------------
(setq split-width-threshold 170 ; so vertical splits are preferred
      split-height-threshold nil
      switch-to-buffer-obey-display-actions t ; so buffer/window rules are respected
      window-combination-resize t
      window-resize-pixelwise nil)

;; Help Window Look and Feel
(setq help-window-select t)

;; Mini-Buffer Look and Feel
(setq read-answer-short t
      use-short-answers t
      enable-recursive-minibuffers t
      resize-mini-windows 'grow-only)

;; ---------------------------------
;; buffer placement behavior
;; ---------------------------------
(add-to-list 'display-buffer-alist
	     '("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Messages\\|Bookmark List\\|Occur\\|eldoc\\)\\*"
	       (display-buffer-in-side-window)
	       (window-height . 0.25)
	       (side . bottom)
	       (slot . 0)))
(add-to-list 'display-buffer-alist
	     '("\\*\\([Hh]elp\\)\\*"
	       (display-buffer-in-side-window)
	       (window-width . 75)
	       (side . right)
	       (slot . 0)))
(add-to-list 'display-buffer-alist
	     '("\\*\\(Ibuffer\\)\\*"
	       (display-buffer-in-side-window)
	       (window-width . 100)
	       (side . right)
	       (slot . 1)))

