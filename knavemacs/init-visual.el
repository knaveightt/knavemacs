;; ==================================================
;;; SECTION 2 Visual Configuration
;; ==================================================

;; --------------------------
;;; 2.1 Overall Look and Feel
;; --------------------------

;; theme configuration
;(load-theme 'deeper-blue t)
;(set-face-attribute 'fringe nil :background "#181a26")
;(set-fringe-mode '(0 . 8)) ; Hides the left fringe (0 width), sets right to 8

;; font configuration
(add-to-list 'default-frame-alist
	     '(font . "GeistMono NF 12"))

;; how scrolling works
(setq scroll-margin 0)
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 101)

;; ------------------------------
;;; 2.2 Line Visual Configuration
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

;; -------------------------------------------
;;; 2.3 Terminal-Specific Visual Configuration
;; -------------------------------------------

;; change vertical separator to full line
;; and truncations to a right arrow
(set-display-table-slot standard-display-table 'vertical-border ?\u2502)
(set-display-table-slot standard-display-table 'truncation ?\u2192)

;; -----------------------------------
;;; 2.4 Frame and Window Look and Feel
;; -----------------------------------

;; Frame and Window General Configuration
(setq frame-resize-pixelwise t
      pixel-scroll-precision-mode t
      pixel-scroll-precision-use-momentum nil
      scroll-conservatively 8
      scroll-margin 0
      split-width-threshold 170 ; so vertical splits are preferred
      split-height-threshold nil
      switch-to-buffer-obey-display-actions t ; so buffer/window rules are respected
      window-combination-resize t
      window-resize-pixelwise nil
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

;; Help Window Specific
(setq help-window-select t)

;; Mini-Buffer Specific
(setq read-answer-short t
      use-short-answers t
      enable-recursive-minibuffers t
      resize-mini-windows 'grow-only)

;; ---------------------------------
;; 2.5 display-buffer-alist Settings
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
