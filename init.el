;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c C-c") #'imenu); eval: (setq imenu-generic-expression '(("Sections" "^;;; \\(.*\\)$" 1))); -*-

;; ===================================================
;;; SECTION 1 Initial Emacs Setup Configuration
;; ===================================================
;; we use utf-8 here
(modify-coding-system-alist 'file "" 'utf-8)

;; initial startup speed hack and frame handling
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))
(setq inhibit-compacting-font-caches t)

;; disable UI elements, inhibit other capabilities
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq ring-bell-function 'ignore)

;; how to handle backup files
(setq create-lockfiles nil)  ; No backup files
(setq make-backup-files nil) ; No backup files
(setq backup-inhibited t)    ; No backup files

;; how to handle custom set vars
(setq custom-file "~/.config/emacs/emacs-custom.el")
(load custom-file t)

;; how to handle recents file
(setq recentf-max-saved-items 300) ; default was 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))

;; how to handle history, kill-ring, undo logic
(setq history-length 300)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring
	register-alist
	mark-ring global-mark-ring
	search-ring regexp-search-ring))
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)
(setq kill-do-not-save-duplicates t)
(setq undo-limit (* 13 160000))
(setq undo-strong-limit (* 13 240000))
(setq undo-outer-limit (* 13 24000000))

;; how to handle searches
(setq xref-search-program 'ripgrep)
(setq grep-command "rg -nS --no-heading")
(setq grep-find-ignored-directories
         '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")

;; how to handle warnings and errors
;; (Avoid raising the *Messages* buffer if anything is still without lexical bindings)
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;; Use-Package and Package Setup
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

;; ===================================================
;;; SECTION 2 Visual Configuration and Logic
;; ===================================================
;; line numbers activation
(setq display-line-numbers-width 3)
(setq display-line-numbers-widen t)
(setq display-line-numbers-type t)
(global-display-line-numbers-mode t)

;; when to show line numbers
(defun knavemacs/no-line-nums-hook ()
  "Supress showing line numbers for select modes."
  (display-line-numbers-mode 0))
(dolist (mode '(term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		dired-mode-hook))
  (add-hook mode 'knavemacs/no-line-nums-hook))

;; Visual Line Handling and Tabs
(set-default 'truncate-lines t)
(setq tab-always-indent 'complete)
(setq tab-width 4)
(setq c-basic-offset 4)

;; Font Configuration
(add-to-list 'default-frame-alist
	     '(font . "JetBrainsMono NF 12"))

;; Theme Configuration
(use-package color-theme-sanityinc-tomorrow
  :ensure t  
  :config
  (color-theme-sanityinc-tomorrow-bright))

;; cursor configuration
(setq-default cursor-type 'bar)

;; Frame and Window Logic & Scrolling
(setq frame-resize-pixelwise t)
(setq pixel-scroll-precision-mode t)
(setq pixel-scroll-precision-use-momentum nil)
(setq scroll-conservatively 8)
(setq scroll-margin 0)
(setq split-width-threshold 170) ; so vertical splits are preferred
(setq split-height-threshold nil)
(setq switch-to-buffer-obey-display-actions t) ; so buffer/window rules are respected
(setq window-combination-resize t)
(setq window-resize-pixelwise nil)
(setq frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))

;; Help things, Mini-Buffer things
(setq help-window-select t)
(setq read-answer-short t)
(setq use-short-answers t)
(setq enable-recursive-minibuffers t)
(setq resize-mini-windows 'grow-ontly)

;; A Protesilaos life savier HACK
;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
;; of the diff (if you choose `d') of what you're asked to save.
(add-to-list 'save-some-buffers-action-alist
             (list "d"
          	   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
          	   "show diff between the buffer and its file"))

;; Terminal-specific configuration
;; On Terminal: changes the vertical separator to a full vertical line
;;              and truncation symbol to a right arrow
(set-display-table-slot standard-display-table 'vertical-border ?\u2502)
(set-display-table-slot standard-display-table 'truncation ?\u2192)

;; display-buffer-alist settings
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
;;; SECTION 3 Mode-Specific Configurations
;; ==================================================
;;; ibuffer-mode
;; --------------------------------------------------
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

;; --------------------------------------------------
;;; org-mode
;; --------------------------------------------------
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

;; --------------------------------------------------
;;; which-key-mode
;; --------------------------------------------------
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
    "SPC g" "Git Commands"
    "SPC t" "Tab Commands"
    "SPC v" "Version Control"
    "SPC x" "Ctrl-X Commands"))

;; --------------------------------------------------
;;; tab-bar-mode
;; --------------------------------------------------
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

;; --------------------------------------------------
;;; tab-line-mode
;; --------------------------------------------------
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

;; --------------------------------------------------
;;; dired-mode
;; --------------------------------------------------
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
;;; SECTION 4 Elisp-Built Functionality
;; ==================================================
;;; rainbow delimiters
;;
;; This is built off of emacs-solo by LionyxML
;; --------------------------------------------------
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

;; --------------------------------------------------
;;; acey window
;;
;; This is built off of emacs-solo by LionyxML
;; --------------------------------------------------
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

;; --------------------------------------------------
;;; highlight keywords
;;
;; This is built off of emacs-solo by LionyxML
;; --------------------------------------------------
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
;;; SECTION 5 Mode-Specific and Global Keybinds
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

;; ===================================================
;;; SECTION 6 {External Packages}
;; ===================================================
;;; {core} vertico completions
;; - vertico
;; - savehist
;; - marginalia
;; - orderless
;; --------------------------------------------------
(use-package vertico
  :ensure t
  :bind (:map minibuffer-local-map
              ("C-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
  :after vertico
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; --------------------------------------------------
;;; {core} nerd-icon packages
;; - nerd-icons
;; - nerd-icons-completions
;; - nerd-icons-dired
;; - nerd-icons-ibuffer
;; - nerd-icons-corfu
;; --------------------------------------------------
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after vertico marginalia nerd-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-dired
  :ensure t
  :init (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :ensure t
  :init (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; --------------------------------------------------
;;; {core} modal editing system
;; --------------------------------------------------
;; - expand-region
;; - multiple-cursors
;; - surround
;; - fzf
;; - avy
;; - ryo-modal
;; --------------------------------------------------
(use-package expand-region
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package surround
  :ensure t)

(use-package fzf
  :ensure t
  :config
  (setq fzf/args "-x --print-query --margin=1,0 --no-hscroll"))

(use-package avy
  :ensure t
  :config
  ;; define an avy action to kill a while line based on a selection
  ;; (see https://karthinks.com/software/avy-can-do-anything/)
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  ;; add custom avy actions to the action dispatcher
  (setf (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
  	))

(use-package ryo-modal
  :ensure t
  :load-path "~/.config/emacs/modal"
  :commands ryo-modal-mode
  :bind
  (("C-z" . ryo-modal-mode)
   ("<escape>" . ryo-modal-mode))
  :config
  (require 'knavemacs-modal-keyfuncs) ; load modal key functions

  ;; special convenience keys for quick actions when entering modal mode
  (ryo-modal-key "C-k" 'kill-current-buffer) ; kill buffers
  (ryo-modal-key "C-M-j" 'ibuffer) ; list all buffers
  (ryo-modal-key "C-j" 'switch-to-buffer) ; list buffers
  
  ;; mapping of existing keymaps to SPC menu,
  ;; along with changes to those maps to support this
  (define-key ryo-modal-mode-map (kbd "SPC x") ctl-x-map)
  (define-key ryo-modal-mode-map (kbd "SPC v") vc-prefix-map)
  (define-key ryo-modal-mode-map (kbd "SPC p") project-prefix-map)
  (define-key ryo-modal-mode-map (kbd "'") surround-keymap)
  (define-key ctl-x-map (kbd "s") #'(lambda () (interactive) (if ryo-modal-mode (save-buffer) (save-some-buffers))))
  (define-key ctl-x-map (kbd "f") #'knavemacs/modal--find-file) ;; needs to be called interactively
  (define-key ctl-x-map (kbd "c") #'save-buffers-kill-terminal)
  (define-key ctl-x-map (kbd "j") #'dired-jump)

  ;; custom menus using SPC+<key> as leaders
  (ryo-modal-key
   "SPC" '(("t t" tab-line-mode)
	   ("t T" tab-bar-mode)
	   ("t j" knavemacs/tab-line-pinned-switch-to-buffer)
	   ("t r" knavemacs/tab-line-pinned-reset-buffers)
	   ("t p" knavemacs/tab-line-pinned-pin-buffer)
	   ("t u" knavemacs/tab-line-pinned-unpin-buffer)
	   ("e e" treemacs-add-and-display-current-project-exclusively)
	   ("g s" magit-status)
  	   ("o c" org-capture)
  	   ("o a" org-agenda)
  	   ("o t" knavemacs/org-quick-time-stamp-inactive)
					("o l" org-store-link)))

  ;; keyboard modal key mappings
  (ryo-modal-keys
   ("," knavemacs/modal--scroll-up-half-page)
   ("." knavemacs/modal--scroll-down-half-page)
   ("\"" surround-insert)
   ("\\" ryo-modal-repeat)
   ("/" fzf-grep-with-narrowing)
   ("?" fzf-grep-in-dir-with-narrowing)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("+"
	(("j"
	  enlarge-window
	  :properties ((repeat-map . knavemacs/modal--window-manage-repeat-map)))
	 ("k"
	  shrink-window
	  :properties ((repeat-map . knavemacs/modal--window-manage-repeat-map)))
	 ("l"
	  enlarge-window-horizontally
	  :properties ((repeat-map . knavemacs/modal--window-manage-repeat-map)))
	 ("h"
	  shrink-window-horizontally
	  :properties ((repeat-map . knavemacs/modal--window-manage-repeat-map)))))
   ("["
    (("t"
      knavemacs/tab-line-pinned-prev-tab)))
   ("]"
    (("t"
      knavemacs/tab-line-pinned-next-tab)))
   ("{" backward-paragraph)
   ("}" forward-paragraph)
   ("a" back-to-indentation :exit t)
   ("A" end-of-line :exit t)
   ("b" exchange-point-and-mark)
   ("B" knavemacs/forward-or-backward-sexp)
   ("c" kill-ring-save) 
   ("C" copy-to-buffer) 
   ("d" knavemacs/modal--dwim-delete)
   ("D" kill-whole-line)
   ("e" knavemacs/modal--increment-expression)
   ("E" knavemacs/modal--decrement-expression)
   ("F" avy-goto-char-timer) 
   ("f" avy-goto-char-in-line)
   ("g" ; _goto_ commands
    (("v"
      knavemacs/modal--jump-back-to-mark)
     ("u"
      universal-argument)
     ))
   ("G" knavemacs/tab-line-pinned-prompt-to-jump)
   ("h" backward-char)
   ("H" beginning-of-line)
   ("i" ryo-modal-mode)
   ("I" delete-region :exit t)
   ("j" next-line)
   ("J" knavemacs/modal--shift-point-bottom)
   ("k" previous-line)
   ("K" knavemacs/modal--shift-point-top)
   ("l" forward-char)
   ("L" end-of-line)
   ("M" move-to-window-line-top-bottom)
   ("m" recenter-top-bottom)
   ("n" er/expand-region)
   ("N" ; smart expand region
    (("q"
      er/mark-inside-quotes)
     ("Q"
      er/mark-outside-quotes)
     ("p"
      er/mark-inside-pairs)
     ("P"
      er/mark-outside-pairs)
     ("d"
      er/mark-defun)
     ("u"
      er/mark-url)
     ("c"
      er/mark-comment)))
   ("o" knavemacs/modal--open-line-below :exit t)
   ("O" knavemacs/modal--open-line-above :exit t)
   ("p" ; multiple points
    (("p"
      mc/mark-all-like-this :mc-all t)
     ("["
      mc/mark-previous-like-this :mc-all t)
     ("]"
      mc/mark-next-like-this :mc-all t)
     ("o"
      mc/mark-pop)))
   ("P" mc/edit-lines :mc-all t)
   ("Q" revert-buffer)
   ("R" delete-region :then '(yank))
   ("r" knavemacs/modal--read-replacement-text)
   ("s" avy-goto-word-1 :then '(set-mark-command forward-word))
   ("S" avy-goto-line :then '(knavemacs/modal--set-mark-line exchange-point-and-mark))
   ("t" ; transform options
    (("u"
      upcase-dwim)
     ("d"
      downcase-dwim)))
   ("T" transpose-lines)
   ("u" undo)
   ("U" undo-redo)
   ("v" knavemacs/modal--set-or-cancel-mark)
   ("V" knavemacs/modal--set-mark-line)
   ("w" forward-word) ; forward word
   ("W" backward-word) ; backward word
   ("x" delete-char) ; delete character
   ("X" backward-delete-char-untabify) ; reverse delete character (backspace)
   ("y" yank) ; yank
   ("Y" yank-pop) ; yank from kill ring (fuzzy select)
   ("z" zap-up-to-char) ; zap up to char
   ("Z" zap-to-char)) ; zap to char

  (ryo-modal-keys
   ;; First argument to ryo-modal-keys may be a list of keywords.
   ;; These keywords will be applied to all keybindings.
   (:norepeat t)
   ("0" "M-0")
   ("1" "M-1")
   ("2" "M-2")
   ("3" "M-3")
   ("4" "M-4")
   ("5" "M-5")
   ("6" "M-6")
   ("7" "M-7")
   ("8" "M-8")
   ("9" "M-9"))

  ;; try to enter modal editing when a new file is visited in a buffer
  (add-hook 'find-file-hook 'ryo-modal-mode))

;; --------------------------------------------------
;;; {programming} corfu/cape completions
;; --------------------------------------------------
(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode 1))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("M-N" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file))
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

;; --------------------------------------------------
;;; {programming} yasnippet packages
;; - yasnippet
;; - yasnippet-capf
;; --------------------------------------------------
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
  	'("~/Documents/data/snippets"))
  (yas-global-mode 1) ;; or M-x yas-reload-all if YASnippet is already started
  )

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; --------------------------------------------------
;;; {programming} diff-hl
;; --------------------------------------------------
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;; --------------------------------------------------
;;; {programming} magit
;; --------------------------------------------------
(use-package magit
  :if (eq system-type 'gnu/linux)
  :ensure t)

;; --------------------------------------------------
;;; {programming} filetree navigation
;; - treemacs
;; - treemacs-magit
;; - treemacs-nerd-icons
;; --------------------------------------------------
(use-package treemacs
  :ensure t
  :config
  (setq treemacs-project-follow-mode t))

(use-package treemacs-magit
  :ensure t)

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-nerd-icons-config))

;; --------------------------------------------------
;;; {visual} rainbow-mode
;; --------------------------------------------------
(use-package rainbow-mode
  :ensure t)

;; --------------------------------------------------
;;; {visual} org-bullets
;; --------------------------------------------------
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis " ⤵"))

;; ==================================================
;;; SECTION X Modeline Configuration
;; ==================================================
(defface knavemacs/modeline-faces-modal
  '((t :foreground "#A720CC"
       :background "#999999"
       :weight bold
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-readonly
  '((t :foreground "#7d0a36"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-modified
  '((t :foreground "#FDB91F"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-unmodified
  '((t :foreground "#4BDD10"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-kmacrorec
  '((t :foreground "#D20103"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-git-additions
  '((t :foreground "#CC0"
       :background "#000000"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-git-deletions
  '((t :foreground "#CC0000"
       :background "#000000"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-header-sep
  '((t :foreground "#999999"
       :background "#444444"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-header-sep2
  '((t :foreground "#444444"
       :background "#111111"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-header-sep3
  '((t :foreground "#111111"
       :background "#030303"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-mid-background
  '((t :background "#000000"
       :foreground "#0011A8"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces) 

(defface knavemacs/modeline-faces-git-branch
  '((t :background "#000000"
       :foreground "#888888"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(set-face-attribute 'mode-line nil :background "#111111")
(set-face-attribute 'mode-line-inactive nil :background "#030303")

;; ------------MODELINE MODULES

;; modeline module: modal indicator
(defvar-local knavemacs/modeline-modal-indicator
  	'(:eval
  	  (when (mode-line-window-selected-p)
  		(propertize (knavemacs/return-modal-state) 'face 'knavemacs/modeline-faces-modal)))
  "Modeline module to show modal / Emacs state indicator.")

;; modeline module: readonly indicator
(defvar-local knavemacs/modeline-readonly-indicator
    '(:eval
  	  (when buffer-read-only
        (propertize " " 'face 'knavemacs/modeline-faces-readonly)))
  "Modeline module to provide a readonly indicator for appropriate buffers")

;; modeline module: modified indicator
(defvar-local knavemacs/modeline-modified-indicator
    '(:eval
      (if (buffer-modified-p)
          (propertize "" 'face 'knavemacs/modeline-faces-modified)
        (propertize "" 'face 'knavemacs/modeline-faces-unmodified))))

;; modeline module: buffer name
(defvar-local knavemacs/modeline-bufname
  	'(:eval
  	  (propertize (buffer-name) 'help-echo (buffer-file-name)))
  "Modeline module to provide the buffer name.")

;; modeline module: major mode icon
(defvar-local knavemacs/modeline-major-mode-icon
    '(:eval
	  (when (mode-line-window-selected-p)
        (nerd-icons-icon-for-mode major-mode)))
  "Modeline module to provide an icon based on the major mode.")

;; modeline module: major mode name
(defvar-local knavemacs/modeline-major-mode-name
    '(:eval
      (when (mode-line-window-selected-p)
        (propertize (concat (format-mode-line mode-name) " ") 'face 'knavemacs/modeline-faces-mid-background)))
  "Modeline module to provide major mode name.")

;; modeline module: right display
(defvar-local knavemacs/modeline-right-display
  	'(""
  	  " L%l:C%c "
  	  "[%p]")
  "Modeline module ot provide minimal modeline info aligned right.")

;; modeline module: kmacro record indicator
(defvar-local knavemacs/modeline-kmacro-indicator
  	'(:eval
  	  (when defining-kbd-macro
        (propertize " (󰑋 MACRO)" 'face 'knavemacs/modeline-faces-kmacrorec)))
  "Modeline module to provide an indicator for when recording kmacros")

;; modeline module: version control details
(defvar-local knavemacs/modeline-vc-details-additions
    '(:eval
	  (when (mode-line-window-selected-p)
        (propertize (knavemacs/return-git-diff-additions) 'face 'knavemacs/modeline-faces-git-additions)))
  "Modeline module to provide version control details.")

;; modeline module: version control details
(defvar-local knavemacs/modeline-vc-details-deletions
    '(:eval
	  (when (mode-line-window-selected-p)
        (propertize (knavemacs/return-git-diff-deletions) 'face 'knavemacs/modeline-faces-git-deletions)))
  "Modeline module to provide version control details.")

;; modeline module: header seperator
(defvar-local knavemacs/modeline-header-sep
    '(:eval
      (when (mode-line-window-selected-p)
	(propertize (knavemacs/modeline-return-sep) 'face 'knavemacs/modeline-faces-header-sep)))
  "Modeline module to simply provide a char for seperation")

(defvar-local knavemacs/modeline-header-sep2
    '(:eval
      (when (mode-line-window-selected-p)
	(propertize (knavemacs/modeline-return-sep) 'face 'knavemacs/modeline-faces-header-sep2)))
  "Modeline module to simply provide a char for seperation")

(defvar-local knavemacs/modeline-header-sep3
    '(:eval
      (when (mode-line-window-selected-p)
	(propertize (knavemacs/modeline-return-sep) 'face 'knavemacs/modeline-faces-header-sep3)))
  "Modeline module to simply provide a char for seperation")

(defvar-local knavemacs/modeline-header-sep4
    '(:eval
      (when (mode-line-window-selected-p)
	(propertize (knavemacs/modeline-return-sep2) 'face 'knavemacs/modeline-faces-header-sep3)))
  "Modeline module to simply provide a char for seperation")

;; modeline module: return git branch name
(defvar-local knavemacs/modeline-gitbranch
    '(:eval
      (when (mode-line-window-selected-p)
	(propertize (knavemacs/modeline-return-git-branch) 'face 'knavemacs/modeline-faces-git-branch)))
  "Modeline module to simply provide a char for seperation")

;; ------------MODELINE PREPARE VARIABLES
(dolist (construct '(knavemacs/modeline-modal-indicator
					 knavemacs/modeline-major-mode-icon
  					 knavemacs/modeline-bufname
					 knavemacs/modeline-readonly-indicator
  					 knavemacs/modeline-modified-indicator
					 knavemacs/modeline-major-mode-name
					 knavemacs/modeline-vc-details-additions
					 knavemacs/modeline-vc-details-deletions
					 knavemacs/modeline-header-sep
					 knavemacs/modeline-header-sep2
					 knavemacs/modeline-header-sep3
					 knavemacs/modeline-header-sep4
					 knavemacs/modeline-gitbranch
  					 knavemacs/modeline-right-display
  					 knavemacs/modeline-kmacro-indicator))
  (put construct 'risky-local-variable t)) ;; required for modeline local vars

;; ------------MODELINE FUNCTIONS
(defun knavemacs/modeline-fill-for-alignment ()
  "Modeline module to provide filler space until right-aligned items are added to modeline."
  (let ((r-length (length (concat
                           "   "
                           (format-mode-line knavemacs/modeline-right-display)
                           (format-mode-line knavemacs/modeline-kmacro-indicator)
                           (format-mode-line knavemacs/modeline-major-mode-name)))))
    (propertize " "
                'display `(space :align-to (- right ,r-length))
                'face 'knavemacs/modeline-faces-mid-background)))

(defun knavemacs/return-modal-state ()
  "Returns the current viper state, or a default string if void."
  (interactive)
  (if ryo-modal-mode
	  (setq modal-mode-string " 󰷈 MEdit ")
	(setq modal-mode-string "  Emacs "))
  (format-mode-line 'modal-mode-string))

;; clocks to throttle the additions/deletions functions to run after some time
(defvar git-modeline-last-update (float-time) "Last time we updated")
(defvar git-modeline-update-interval 15 "Minimum time between update in seconds")
(defvar git-modeline-toggle t "True if additions/deletions should be added to modeline")
(defvar git-modeline-additions "")
(defvar git-modeline-deletions "")

(defun knavemacs/return-git-diff-additions ()
  "Return a string showing the number of added for the current file."
  (if (and (> (- (float-time) git-modeline-last-update) git-modeline-update-interval) git-modeline-toggle)
      (progn
        (when (buffer-file-name)
          (let ((file (buffer-file-name)))
            (when (vc-backend file)
              (when (eq (vc-backend file) 'Git)
                (let ((diff-output (vc-git--run-command-string file "diff" "--numstat" "--" file)))
                  (when (and diff-output (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" diff-output))
                    (setq git-modeline-additions (format "  %s" (match-string 1 diff-output)))))))))
        ; we comment this out and expect the deletions function to reset the clock
        ; (setq git-modeline-last-update (float-time))
        (when (not (buffer-file-name))
          (setq git-modeline-additions ""))
        )
    (if (> (- (float-time) git-modeline-last-update) git-modeline-update-interval) (setq git-modeline-additions "")))
  (format-mode-line 'git-modeline-additions))

(defun knavemacs/return-git-diff-deletions ()
  "Return a string showing the number of added for the current file."
  (if (and (> (- (float-time) git-modeline-last-update) git-modeline-update-interval) git-modeline-toggle)
      (progn
        (when (buffer-file-name)
          (let ((file (buffer-file-name)))
            (when (vc-backend file)
              (when (eq (vc-backend file) 'Git)
                (let ((diff-output (vc-git--run-command-string file "diff" "--numstat" "--" file)))
                  (when (and diff-output (string-match "^\\([0-9]+\\)\t\\([0-9]+\\)\t" diff-output))
                    (setq git-modeline-deletions (format "  %s" (match-string 2 diff-output)))))))))
        (when (not (buffer-file-name))
          (setq git-modeline-deletions ""))
	(setq git-modeline-last-update (float-time)))
    (if (> (- (float-time) git-modeline-last-update) git-modeline-update-interval)
	(progn
	  (setq git-modeline-deletions "")
	  (setq git-modeline-last-update (float-time)))))
  (format-mode-line 'git-modeline-deletions))

(defun knavemacs/modeline-return-sep ()
  "Return a seperator to be used in the modeline."
  "")

(defun knavemacs/modeline-return-sep2 ()
  "Return a seperator to be used in the modeline."
  "")

(defun knavemacs/modeline-return-git-branch ()
  "Return the Git branch when in a versioned controlled file."
  (interactive)
  (when vc-mode
    (let* ((gbranch (elt (split-string vc-mode ":") 1))
	   (mdln (concat "  " gbranch)))
      mdln)))

;; ------------modeline CONSTRUCTION
(setq-default mode-line-format
  			  '("%e"
  				;mode-line-front-space
  				knavemacs/modeline-modal-indicator
				knavemacs/modeline-header-sep
				knavemacs/modeline-header-sep2
                " "
  				knavemacs/modeline-major-mode-icon
                " "
  				knavemacs/modeline-bufname
  				knavemacs/modeline-readonly-indicator
  				" "
  				knavemacs/modeline-modified-indicator
  				" "
				knavemacs/modeline-header-sep3
				knavemacs/modeline-gitbranch
				knavemacs/modeline-vc-details-additions
				knavemacs/modeline-vc-details-deletions
  				(:eval (knavemacs/modeline-fill-for-alignment))
  				knavemacs/modeline-major-mode-name
				knavemacs/modeline-header-sep4
  				knavemacs/modeline-right-display
  				knavemacs/modeline-kmacro-indicator))

;; ==================================================
;;; SECTION Y Final Startup Sequence
;; ==================================================
;; Setup these minor modes:
(global-auto-revert-mode 1)
(indent-tabs-mode -1)
(electric-pair-mode 1)
(recentf-mode 1)
(repeat-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(winner-mode)
(xterm-mouse-mode 1)
(delete-selection-mode 1)
(file-name-shadow-mode 1) ; allows us to type a new path without having to delete the current one

;; clean up and notify
(if (eq system-type 'gnu/linux) (shell-command "notify-send 'Emacs Configuration Loaded'"))

;; ==================================================
;;; SECTION Z Platform-Specific Configration
;; ==================================================
(add-to-list 'load-path "~/.config/emacs/platform")
(require 'knavemacs-platform)







