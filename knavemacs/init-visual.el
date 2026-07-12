;; init-visual.el - Knavemacs Base Visual Customizations (Look and Feel)
;; Part of Knavemacs

;; --------------------------------
;; general visual theme definitions
;; --------------------------------
;; font configuration when in graphical mode
(add-to-list 'default-frame-alist
	     '(font . "GeistMono NF 12"))

;; starting theme config
(load-theme 'modus-vivendi-deuteranopia t)

;; further theme configuration
;(set-face-attribute 'fringe nil :background "#181a26")
;(set-fringe-mode '(0 . 8)) ; Hides the left fringe (0 width), sets right to 8
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

;; -----------------------------------
;; Frame and Window scroll and selection behavior
;; -----------------------------------
;; general options
(setq frame-resize-pixelwise t
      pixel-scroll-precision-mode t
      pixel-scroll-precision-use-momentum nil
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      split-width-threshold 170 ; so vertical splits are preferred
      split-height-threshold nil
      switch-to-buffer-obey-display-actions t ; so buffer/window rules are respected
      window-combination-resize t
      window-resize-pixelwise nil
      frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

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
