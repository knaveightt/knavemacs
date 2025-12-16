;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c C-c") #'imenu); eval: (setq imenu-generic-expression '(("Sections" "^;;; \\(.*\\)$" 1))); -*-

;; document dependencies
;; - patched font (jet brains mono nerd font)
;; - ripgrep
;; 

;; ==================================================
;;; SECTION 1 Core Emacs Configuration
;; ==================================================

;; --------------------------
;;; 1.1 Startup Configuration
;; --------------------------

;; initial startup speed hack and frame handling
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))
(setq inhibit-compacting-font-caches t)

;; disable UI and startup elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t
      use-dialog-box nil
      use-file-dialog nil
      ring-bell-function 'ignore)

;; ---------------------------------
;;; 1.2 Auxiliary File Configuration
;; ---------------------------------

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

;; --------------------------
;;; 1.3 Search Configurations
;; --------------------------
(setq isearch-lazy-count t
      lazy-count-prefix-format "(%s/%s) "
      lazy-count-suffix-format nil
      search-whitespace-regexp ".*?")
(setq xref-search-program 'ripgrep
      grep-command "rg -nS --no-heading"
      grep-find-ignored-directories
               '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

;; -------------------------------------
;;; 1.4 Undo and Kill Ring Configuration
;; -------------------------------------
(setq undo-limit (* 13 160000)
      undo-strong-limit (* 13 240000)
      undo-outer-limit (* 13 24000000))
(setq kill-do-not-save-duplicates t)

;; --------------------------------------
;;; 1.5 Warnings and Errors Configuration
;; --------------------------------------
(setq warning-minimum-level :error
      warning-suppress-types '((lexical-binding)))

;; --------------------------------------
;;; 1.6 Tab and Indentation Configuration
;; --------------------------------------
(setq tab-always-indent 'complete)
(setq-default indent-tabs-mode nil) ; spaces only
(setq tab-width 4)

;; ------------------------
;;; 1.7 Pairs Configuration
;; ------------------------
(electric-pair-mode 1)

;; ==================================================
;;; SECTION 2 Visual Configuration
;; ==================================================

;; --------------------------
;;; 2.1 Overall Look and Feel
;; --------------------------

;; theme configuration
(load-theme 'deeper-blue t)
(set-face-attribute 'fringe nil :background "#181a26")
(set-fringe-mode '(0 . 8)) ; Hides the left fringe (0 width), sets right to 8

;; font configuration
(add-to-list 'default-frame-alist
	     '(font . "JetBrainsMono NF 12"))

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


;; ==================================================
;;; SECTION 3 Use-Package Configuration and Setup
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
;;; SECTION 4 Mode-Specific Configuration
;; ==================================================

;; -----------------
;;; 4.1 ibuffer-mode
;; -----------------
(setq ibuffer-saved-filter-groups
      '(("default"
         ("org" (or
          	 (mode . org-mode)
          	 (name . "^\\*Org Src")
          	 (name . "^\\*Org Agenda\\*$")))
         ("tramp" (name . "^\\*tramp.*"))
         ("emacs" (or
          	   (name . "^\\*scratch\\*$")
          	   (name . "^\\*Messages\\*$")
          	   (name . "^\\*Warnings\\*$")
          	   (name . "^\\*Shell Command Output\\*$")
          	   (name . "^\\*Async-native-compile-log\\*$")
          	   (name . "^\\*straight-")))
         ("ediff" (or
          	   (name . "^\\*ediff.*")
          	   (name . "^\\*Ediff.*")))
         ("dired" (mode . dired-mode))
         ("terminal" (or
          	      (mode . term-mode)
          	      (mode . shell-mode)
          	      (mode . eshell-mode)))
         ("help" (or
          	  (name . "^\\*Help\\*$")
          	  (name . "^\\*info\\*$")
          	  (name . "^\\*helpful"))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))
(setq ibuffer-show-empty-filter-groups nil) ; don't show empty groups
:init
(set-window-margins (selected-window) 2 0)

;; -------------
;;; 4.2 org-mode
;; -------------
(use-package org
  :config
  (setq org-agenda-files (list "~/Documents/org" "~/Documents/org/areas"))
  (setq org-agenda-todo-list-sublevels nil) ;; only want to see top level TODOs in global list
  (setq org-stuck-projects '("+TODO=\"PROJECT\"" ("TODO" "FOLLOWUP")))
  (setq org-refile-targets '((org-agenda-files :level . 1)))
  (setq org-id-link-to-org-use-id t)
  (setq org-todo-keywords
	'((sequence "BACKLOG(b)" "TODO(t)" "NEXT(n)" "PROJECT(p)" "FOLLOWUP(w@)" "|" "DONE(d!)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
	'(("BACKLOG" . org-warning)
          ("TODO" . org-todo)
          ("NEXT" . org-todo)
          ("PROJECT" . org-drawer)
          ("FOLLOWUP" . org-macro)
          ("DONE" . org-done)
          ("CANCELLED" . org-property-value)
          ))

  ;; custom agenda views
  (setq org-agenda-custom-commands
	'(
	  ("d" "Todo Planning"
	   (
	    (agenda ""
		    ((org-deadline-warning-days 7)
		     (org-agenda-overriding-header "Scheduled TODOs")))
	    (tags "+TODO=\"TODO\"-SCHEDULED={.+}|+SCHEDULED=\"<today>\""
		  ((org-agenda-overriding-header "Today's Work")))
		(tags "+SCHEDULED<\"<today>\"-TODO=\"DONE\""
		  ((org-agenda-overriding-header "Late Work")))
	    (stuck "" ((org-agenda-overriding-header "Stuck Projects")))
	    (tags "+TODO=\"FOLLOWUP\"-SCHEDULED={.+}"
		  ((org-agenda-overriding-header "Floating Follow-Ups")))
	    ))
          ))

  ;; org function for printing out a quick timestamp
  (defun knavemacs/org-quick-time-stamp-inactive ()
    "Insert an inactive time stamp of the current time without user prompt"
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-time-stamp-inactive))
    (insert " "))

  ;; capture templates
  (setq org-capture-templates
	'(
          ("t" "Todo" entry (file+olp "~/Documents/org/todos.org" "TODOs")
           "* %^{Enter Context} %^G\n** %^{Task Type|TODO|PROJECT} %?\n" :empty-lines-after 1)

          ("q" "Quick Task" entry (file+olp "~/Documents/org/todos.org" "TODOs" "Quick Tasks")
           "* TODO %?\n" :empty-lines-after 1)

          ("f" "Future Todo" entry (file+olp "~/Documents/org/todos.org" "TODOs" "Future Tasks")
           "* TODO %?\n" :empty-lines-after 1)

          ("m" "Meeting Notes" entry (file+olp "~/Documents/org/todos.org" "Meeting Notes")
           "* %t %^{Meeting Title} %^G\n** Attendance\n|Attendee|Present|\n|-|-|\n|%?\n** Notes\n** Action Items\n*** (Begin Todos) " :empty-lines-after 1)

	      ("w" "Work Notes" entry (file+olp "~/Documents/org/todos.org" "Work Notes")
           "* %? %^G\n" :empty-lines-after 1)
          )))

;; -------------------
;;; 4.3 which-key-mode
;; -------------------
(use-package which-key
  :defer t
  :ensure nil
  :hook
  (after-init-hook . which-key-mode)
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  (setq which-key-idle-delay 1.5)
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40)

  (which-key-add-key-based-replacements
    "SPC o" "Org Commands"
    "SPC e" "File Explore Commands"
    "SPC p" "Project Commands"
    "SPC r" "Register Commands"
    "SPC g" "Git Commands"
    "SPC t" "Tab Commands"
    "SPC v" "Version Control"
    "SPC x" "Ctrl-X Commands"))

;; -----------------
;;; 4.4 tab-bar-mode
;; -----------------
(use-package tab-bar
  :ensure nil
  :defer t
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width t)
  (tab-bar-auto-width-min '(10 4))
  (tab-bar-auto-width-max '(50 5))
  :init
  ;; HACK this is an override of the internal function so it
  ;;      shows only the hint number with some decoration.
  (defun tab-bar-tab-name-format-hints (name _tab i)
    "Show absolute numbers on tabs in the tab bar before the tab name.
  It has effect when `tab-bar-tab-hints' is non-nil."
    (if tab-bar-tab-hints (concat (format " »%d«" i) "") name)))

;; ------------------
;;; 4.5 tab-line-mode
;; ------------------
;; I use the tab-line as a way to "pin" buffers for quick
;; visiting, I have functions to pin / unpin buffers to the
;; tab line, as well as to cycle in a way that allows me
;; to quickly access the buffers that have been pinned to the
;; tab line. I can also call a function that allows me to
;; switch to a buffer that is represented on the tab line
;; only. 
(use-package tab-line
  :ensure t
  :config
  ;; set the function and variables used to keep track of pinned buffers
  (setq tab-line-tabs-function 'knavemacs/tab-line-pinned-buffers)
  (setq knavemacs/tab-line-buffers-list (list (current-buffer)))
  (defun knavemacs/tab-line-pinned-buffers ()
    "Provides a list containing buffers that have been explicitly set to show on the tab line"
    knavemacs/tab-line-buffers-list)

  ;; pin buffer to tab line
  (defun knavemacs/tab-line-pinned-pin-buffer ()
    "Pins the current buffer to the tab buffer list"
    (interactive)
    (if (not (seq-contains-p knavemacs/tab-line-buffers-list (current-buffer)))
	(setq knavemacs/tab-line-buffers-list (append knavemacs/tab-line-buffers-list (list (current-buffer)))))
    ;; buffer must have a buffer name. Some dired or other system buffers do not have a name, so filter those out
    (setq knavemacs/tab-line-buffers-list (seq-remove (lambda (elt) (not (buffer-name elt))) knavemacs/tab-line-buffers-list)) 
    (set-window-parameter nil 'tab-line-cache nil) ; for updating
    (force-mode-line-update))

  ;; unpin buffer to tab line
  (defun knavemacs/tab-line-pinned-unpin-buffer ()
    "Removes the current buffer from the tab buffer list"
    (interactive)
    (if (seq-contains-p knavemacs/tab-line-buffers-list (current-buffer))
	(setq knavemacs/tab-line-buffers-list (delete (current-buffer) knavemacs/tab-line-buffers-list)))
    (set-window-parameter nil 'tab-line-cache nil) ; for updating
    (force-mode-line-update))

  ;; switch to a pinned buffer (uses completions)
  (defun knavemacs/tab-line-pinned-switch-to-buffer ()
    "Switches to a buffer that is explicitly pinned to the tab-line"
    (interactive)
    (switch-to-buffer (completing-read "Choose Tab:" (mapcar 'buffer-name (knavemacs/tab-line-pinned-buffers)))))

  ;; I dont typically call this directly, but jump to the last pinned buffer
  (defun knavemacs/tab-line-pinned-switch-to-last ()
    "Automatically switches the active buffer to the last pinned buffer in the tab line."
    (interactive)
    (let ((num-buffers (length knavemacs/tab-line-buffers-list)))
      (setq bufindx (- num-buffers 1))
      (switch-to-buffer (nth bufindx knavemacs/tab-line-buffers-list))))

  ;; I dont typically call this directly, but jump to the first pinned buffer
  (defun knavemacs/tab-line-pinned-switch-to-first ()
    "Automatically switches the active buffer to the first pinned buffer in the tab line."
    (interactive)
    (switch-to-buffer (nth 0 knavemacs/tab-line-buffers-list)))

  ;; used with hotkeys to jump to a specific tab
  (defun knavemacs/tab-line-pinned-switch-to-nth (tabnum)
    "Switch to a specifically numbered tab in tab-line"
    (interactive)
    (if (> tabnum (length knavemacs/tab-line-buffers-list))
	(message "!- That Tab Does Not Exist")
      (switch-to-buffer (nth (1- tabnum) knavemacs/tab-line-buffers-list))))

  ;; get user input for which tab to jump to
  (defun knavemacs/tab-line-pinned-prompt-to-jump (numeric-prefix-arg)
    "Jumps to a specific tab depending on the universal argument value"
    (interactive "p")
    (knavemacs/tab-line-pinned-switch-to-nth numeric-prefix-arg))
	     
  ;; reset the pinned buffer list
  (defun knavemacs/tab-line-pinned-reset-buffers ()
    "Reduce the buffers pinned to the tab line to just the current buffer"
    (interactive)
    (setq knavemacs/tab-line-buffers-list (list (current-buffer))))

  ;; cycle forward the tabs of pinned buffers
  (defun knavemacs/tab-line-pinned-next-tab ()
    "Cycle to the next tab-line tab, selecting the first if no tab is selected"
    (interactive)
    (if (seq-contains-p knavemacs/tab-line-buffers-list (current-buffer))
	(tab-line-switch-to-next-tab)
      (knavemacs/tab-line-pinned-switch-to-first)))

  ;; cycle backwards the tabs of pinned buffers:
  (defun knavemacs/tab-line-pinned-prev-tab ()
    "Cycle to the previous tab-line tab, selecting the last if no tab is selected"
    (interactive)
    (if (seq-contains-p knavemacs/tab-line-buffers-list (current-buffer))
	(tab-line-switch-to-prev-tab)
      (knavemacs/tab-line-pinned-switch-to-last))))

;; ---------------
;;; 4.6 dired-mode
;; ---------------
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open")
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open")
     (".*" "xdg-open" "open")))
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-omit-files "^\\.")                                ; with dired-omit-mode (C-x M-o)
  (dired-hide-details-hide-absolute-location t)            ; EMACS-31
  :init
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1))) ;; Turning this ON also sets the C-x M-o binding.

  (defun knavemacs/window-dired-vc-root-left (&optional directory-path)
    "Creates *Dired-Side* like an IDE side explorer"
    (interactive)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)

    (let ((dir (if directory-path
                   (dired-noselect directory-path)
                 (if (eq (vc-root-dir) nil)
                     (dired-noselect default-directory)
                   (dired-noselect (vc-root-dir))))))

      (display-buffer-in-side-window
       dir `((side . left)
             (slot . 0)
             (window-width . 30)
             (window-parameters . ((no-other-window . t)
                                   (no-delete-other-windows . t)
                                   (mode-line-format . (" "
                                                        "%b"))))))
      (with-current-buffer dir
        (let ((window (get-buffer-window dir)))
          (when window
            (select-window window)
            (rename-buffer "*Dired-Pane*")
            )))))

  (defun knavemacs/window-dired-open-directory ()
    "Open the current directory in *Dired-Side* side window."
    (interactive)
    (knavemacs/window-dired-vc-root-left (dired-get-file-for-visit)))

  ;; In dired mode, visit the file at the cursor in the right/below/left/above window.
  ;; https://news.ycombinator.com/item?id=44075388
  (defun knavemacs/dired-open-display-direction ()
    (interactive)
    (let* ((file-or-dir (dired-get-file-for-visit))   ;; get the file at cursor
           (buffer (find-file-noselect file-or-dir))) ;; load the file into a buffer
      (let ((window                                   ;; figure out the window to use
             (cond ((get-buffer-window buffer (selected-frame)))
                   ((window-in-direction 'right))     ;; try window in each direction
                   ((window-in-direction 'below))     ;; and default to right
                   ((window-in-direction 'left))      ;; if no window found.
                   ((window-in-direction 'above))
                   (t (split-window (selected-window) nil 'right)))))
        (window--display-buffer buffer window 'window nil)
        window))
    (knavemacs/quick-window-jump)))

;; ==================================================
;;; SECTION 5 Elisp-Built Functionality
;; ==================================================

;; -----------------------
;;; 5.1 rainbow delimiters
;; -----------------------

;; This is built off of emacs-solo by LionyxML
(defun knavemacs/rainbow-delimiters ()
  "Apply simple rainbow coloring to parentheses, brackets, and braces in the current buffer.
  Opening and closing delimiters will have matching colors."
  (interactive)
  (let ((colors '(font-lock-keyword-face
                  font-lock-type-face
                  font-lock-function-name-face
                  font-lock-variable-name-face
                  font-lock-constant-face
                  font-lock-builtin-face
                  font-lock-string-face
                  )))
    (font-lock-add-keywords
     nil
     `((,(rx (or "(" ")" "[" "]" "{" "}"))
        (0 (let* ((char (char-after (match-beginning 0)))
                  (depth (save-excursion
                           ;; Move to the correct position based on opening/closing delimiter
                           (if (member char '(?\) ?\] ?\}))
                               (progn
                                 (backward-char) ;; Move to the opening delimiter
                                 (car (syntax-ppss)))
                             (car (syntax-ppss)))))
                  (face (nth (mod depth ,(length colors)) ',colors)))
             (list 'face face)))))))
  (font-lock-flush)
  (font-lock-ensure))
(add-hook 'prog-mode-hook #'knavemacs/rainbow-delimiters)

;; ----------------
;;; 5.2 acey window
;; ----------------

;; This is built off of emacs-solo by LionyxML
(defvar knavemacs/acey-window-quick-window-overlays nil
  "List of overlays used to temporarily display window labels.")

(defun knavemacs/quick-window-jump ()
  "If there are only two windows, jump to the other. Otherwise, initiate acey window jumping"
  (interactive)
  (if (= (length (window-list)) 2)
      (call-interactively 'other-window)
    (knavemacs/acey-window-quick-window-jump)))

(defun knavemacs/acey-window-quick-window-jump ()
  "Jump to a window by typing its assigned character label.
  Windows are labeled starting from the top-left window and proceeding top to bottom, then left to right."
  (interactive)
  (let* ((window-list (knavemacs/acey-window-get-windows))
         (window-keys (seq-take '("1" "2" "3" "4" "5" "6" "7" "8")
                                (length window-list)))
         (window-map (cl-pairlis window-keys window-list)))
    (knavemacs/acey-window-add-window-key-overlays window-map)
    (let ((key (read-key (format "Select window [%s]: " (string-join window-keys ", ")))))
      (knavemacs/acey-window-remove-window-key-overlays)
      (if-let* ((selected-window (cdr (assoc (char-to-string key) window-map))))
          (select-window selected-window)
        (message "No window assigned to key: %c" key)))))

(defun knavemacs/acey-window-get-windows ()
  "Return a list of windows in the current frame, ordered from top to bottom, left to right."
  (sort (window-list nil 'no-mini)
        (lambda (w1 w2)
          (let ((edges1 (window-edges w1))
                (edges2 (window-edges w2)))
            (or (< (car edges1) (car edges2)) ; Compare top edges
                (and (= (car edges1) (car edges2)) ; If equal, compare left edges
                     (< (cadr edges1) (cadr edges2))))))))

(defun knavemacs/acey-window-add-window-key-overlays (window-map)
  "Add temporary overlays to windows with their assigned key labels from WINDOW-MAP."
  (setq knavemacs/acey-window-quick-window-overlays nil)
  (dolist (entry window-map)
    (let* ((key (car entry))
           (window (cdr entry))
           (start (window-start window))
           (overlay (make-overlay start start (window-buffer window))))
      (overlay-put overlay 'after-string
                   (propertize (format " [%s] " key)
                               'face '(:foreground "#c3e88d"
                                                   :background "#232635"
                                                   :weight bold
                                                   :height default)))
      (overlay-put overlay 'window window)
      (push overlay knavemacs/acey-window-quick-window-overlays))))

(defun knavemacs/acey-window-remove-window-key-overlays ()
  "Remove all temporary overlays used to display key labels in windows."
  (mapc 'delete-overlay knavemacs/acey-window-quick-window-overlays)
  (setq knavemacs/acey-window-quick-window-overlays nil))

;; -----------------------
;;; 5.3 highlight keywords
;; -----------------------

;; This is built off of emacs-solo by LionyxML
(defface knavemacs/HL-hack
  '((t :foreground "#221111" :background "#ff4411" :weight bold))
  "Face for HACK tags."
  :group 'knavemacs/highlight-faces)

(defface knavemacs/HL-todo
  '((t :foreground "#AAA" :background "#2233FF" :weight bold))
  "Face for TODO tags."
  :group 'knavemacs/highlight-faces)

(defface knavemacs/HL-fixme
  '((t :foreground "#221111" :background "#F9E900" :weight bold))
  "Face for FIXME tags."
  :group 'knavemacs/highlight-faces)

(defface knavemacs/HL-note
  '((t :foreground "#221111" :background "#22CC33" :weight bold))
  "Face for NOTE tags."
  :group 'knavemacs/highlight-faces)

(defcustom +highlight-keywords-faces
  '(("TODO" . knavemacs/HL-todo)
    ("FIXME" . knavemacs/HL-fixme)
    ("HACK" . knavemacs/HL-hack)
    ("NOTE" . knavemacs/HL-note))
  "Alist of keywords to highlight and their face."
  :group '+highlight-keywords
  :type '(alist :key-type (string :tag "Keyword")
                :value-type (symbol :tag "Face"))
  :set (lambda (sym val)
         (dolist (face (mapcar #'cdr val))
           (unless (facep face)
             (error "Invalid face: %s" face)))
         (set-default sym val)))

(defvar +highlight-keywords--keywords
  (when +highlight-keywords-faces
    (let ((keywords (mapcar #'car +highlight-keywords-faces)))
      `((,(regexp-opt keywords 'words)
         (0 (when (nth 8 (syntax-ppss))
              (cdr (assoc (match-string 0) +highlight-keywords-faces)))
            prepend)))))
  "Keywords and corresponding faces for `knavemacs/highlight-keywords-mode'.")

(defun knavemacs/highlight-keywords-mode-on ()
  (font-lock-add-keywords nil +highlight-keywords--keywords t)
  (font-lock-flush))

(defun knavemacs/highlight-keywords-mode-off ()
  (font-lock-remove-keywords nil +highlight-keywords--keywords)
  (font-lock-flush))

(define-minor-mode knavemacs/highlight-keywords-mode
  "Highlight TODO and similar keywords in comments and strings."
  :lighter " +HL"
  :group '+highlight-keywords
  (if knavemacs/highlight-keywords-mode
      (knavemacs/highlight-keywords-mode-on)
    (knavemacs/highlight-keywords-mode-off)))

(defun knavemacs/highlight-keywords-hook ()
  "Function that runs on a hook to highlight keywords after a moment."
  (run-at-time "1 sec" nil #'knavemacs/highlight-keywords-mode-on))

(add-hook 'prog-mode-hook #'knavemacs/highlight-keywords-hook)

;; ==================================================
;;; SECTION 6 Mode-Specific and Global Keybinds
;; ==================================================
(define-key dired-mode-map (kbd "C-<return>") 'knavemacs/window-dired-open-directory)
(define-key dired-mode-map (kbd "C-k") 'kill-current-buffer)
(define-key dired-mode-map (kbd "C-o") 'knavemacs/dired-open-display-direction)
(define-key dired-mode-map (kbd "C-i") 'dired-kill-subdir)
(global-set-key (kbd "M-o") #'knavemacs/quick-window-jump)
(global-set-key (kbd "M-p") #'knavemacs/window-dired-vc-root-left)
(global-set-key (kbd "S-TAB") #'completion-at-point)
(global-set-key (kbd "S-<iso-lefttab>") #'completion-at-point)
(global-set-key (kbd "M-g r") #'recentf)
(global-set-key (kbd "M-s g") #'grep)
(global-set-key (kbd "C-x ;") #'comment-line)
(global-set-key (kbd "RET") #'newline-and-indent)

;; ==================================================
;;; SECTION 7 Auto-Load External Configuration Files
;; ==================================================

;; function to load .el files in a specific directory
(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (file (directory-files directory))
    (when (string-match "[A-Za-z0-9_-]+\\.el$" file)
      (load (expand-file-name file directory)))))

;; load external "core" packages
(load-directory (expand-file-name "knavemacs_core/" user-emacs-directory))

;; load external "programming" packages
(load-directory (expand-file-name "knavemacs_programming/" user-emacs-directory))

;; load external "visual" packages
(load-directory (expand-file-name "knavemacs_visual/" user-emacs-directory))

;; load external modeline module (internally built)
(load-directory (expand-file-name "knavemacs_modeline/" user-emacs-directory))

;; ==================================================
;;; SECTION 8 Platform-Specific Configuration
;; ==================================================

;; clean up and notify
(if (eq system-type 'gnu/linux) (shell-command "notify-send 'Emacs Configuration Loaded'"))

;; platform specific load files
(setq platform-files (expand-file-name "platform" user-emacs-directory))
(add-to-list 'load-path platform-files)
(require 'knavemacs-platform)
