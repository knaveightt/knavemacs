;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c i") #'imenu); eval: (setq imenu-generic-expression '(("Sections" "^;;; \\(.*\\)$" 1))); -*-

;; ===================================================
;;; SECTION 1 Initial Setup Configuration
;; ===================================================
;; HACK: increase startup speed
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))

;; Better Window Management Handling
(setq frame-inhibit-implied-resize t
      frame-title-format '("Emacs"))
(setq inhibit-compacting-font-caches t)

;; Disable unused UI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message ";; Welcome back to Emacs!")

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setq warning-minimum-level :error)
(setq warning-suppress-types '((lexical-binding)))

;; utf-8 settings
(modify-coding-system-alist 'file "" 'utf-8)

;; History Configuration
(setq history-length 300)
(setq savehist-save-minibuffer-history t)
(setq savehist-additional-variables
      '(kill-ring
	register-alist
	mark-ring global-mark-ring
	search-ring regexp-search-ring))
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
(setq save-place-limit 600)

;; Help Configuration
(setq help-window-select t)

;; Kill Ring Configuration
(setq kill-do-not-save-duplicates t)

;; Backup File Configuration
(setq create-lockfiles nil)  ; No backup files
(setq make-backup-files nil) ; No backup files
(setq backup-inhibited t)    ; No backup files

;; Custom File Management
(setq custom-file "~/.config/emacs/emacs-custom.el")
(load custom-file t)

;; Recent File Configuration
(setq recentf-max-saved-items 300) ; default was 20
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300 'never))
(setq recentf-exclude (list "^/\\(?:ssh\\|su\\|sudo\\)?:"))

;; Prompt and Minibuffer Configuration
(setq read-answer-short t)
(setq use-short-answers t)
(setq enable-recursive-minibuffers t)
(setq resize-mini-windows 'grow-ontly)

;; Search Configuration
(setq xref-search-program 'ripgrep)
(setq grep-command "rg -nS --no-heading")
(setq grep-find-ignored-directories
         '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))
(setq isearch-lazy-count t)
(setq lazy-count-prefix-format "(%s/%s) ")
(setq lazy-count-suffix-format nil)
(setq search-whitespace-regexp ".*?")

;; Terminal-specific configuration
;; On Terminal: changes the vertical separator to a full vertical line
;;              and truncation symbol to a right arrow
(set-display-table-slot standard-display-table 'vertical-border ?\u2502)
(set-display-table-slot standard-display-table 'truncation ?\u2192)

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

;; ==================================================
;;; SECTION 2 General Visual Setup
;; ==================================================
;; Line Numbers
(setq display-line-numbers-width 3)
(setq display-line-numbers-widen t)
(setq display-line-numbers-type t)
(global-display-line-numbers-mode t)
(defun knavemacs/no-line-nums-hook ()
  "Supress showing line numbers for select modes."
  (display-line-numbers-mode 0))
(dolist (mode '(term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		dired-mode-hook))
  (add-hook mode 'knavemacs/no-line-nums-hook))

;; Font and Theme Configuration
(load-theme 'modus-vivendi-deuteranopia t)
(set-cursor-color "#b4d273")
(add-to-list 'default-frame-alist
	     '(font . "JetBrainsMono NF 12"))

;; Visual Line Handling
(set-default 'truncate-lines t)

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

;; Dialogs and Menus
(setq use-dialog-box nil)
(setq use-file-dialog nil)

;; Visual Bell
(setq ring-bell-function 'ignore)

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
;;; SECTION 3 General Editing Setup
;; ==================================================
;; Selection Logic
(setq delete-selection-mode 1)

;; Tab logic
(setq tab-always-indent 'complete)
(setq tab-width 4)
(setq c-basic-offset 4)

;; Undo Logic
(setq undo-limit (* 13 160000))
(setq undo-strong-limit (* 13 240000))
(setq undo-outer-limit (* 13 24000000))

;; A Protesilaos life savier HACK
;; Add option "d" to whenever using C-x s or C-x C-c, allowing a quick preview
;; of the diff (if you choose `d') of what you're asked to save.
(add-to-list 'save-some-buffers-action-alist
             (list "d"
          	   (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
          	   "show diff between the buffer and its file"))

;; ==================================================
;;; SECTION 4 Mode-Specific Configurations
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
  (setq org-stuck-projects '("+TODO=\"PROJECT\"" ("TODO" "NEXT")))
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
          ("d" "Todo Dashboard"
           (
	    (agenda ""
		    ((org-deadline-warning-days 7)
		     (org-agenda-overriding-header "Scheduled Items")))
	    (tags "+@priority-@step+TODO=\"TODO\"-SCHEDULED={.+}|+@priority+@step+TODO=\"NEXT\"-SCHEDULED={.+}"
		  ((org-agenda-overriding-header "Priority Project Work")))
	    (tags "-@priority-@step+TODO=\"TODO\"-SCHEDULED={.+}|-@priority+@step+TODO=\"NEXT\"-SCHEDULED={.+}"
		  ((org-agenda-overriding-header "Available Open Work")))
	    (stuck "" ((org-agenda-overriding-header "Stuck Projects")))
	    (tags "+TODO=\"FOLLOWUP\"-SCHEDULED={.+}"
		  ((org-agenda-overriding-header "Requires Follow Up")))
	    ))
          ))

  ;; org function for printing out a quick timestamp
  (defun knavemacs/org-quick-time-stamp-inactive ()
    "Insert an inactive time stamp of the current time without user prompt"
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-time-stamp-inactive))
    (insert " "))

  ;; org function and advice for leaving a link behind when refiling to another org file
  ;; very slightly modified from the below:
  ;; https://emacs.stackexchange.com/questions/47011/org-refile-and-leave-a-link-behind
  (defun knavemacs/org-refile--insert-link ( &rest _ )
    (org-back-to-heading)
    (let* ((refile-region-marker (point-marker))
           (source-link (org-store-link nil)))
      (org-insert-heading-after-current)
      (insert source-link)
      (goto-char refile-region-marker)))

  (advice-add 'org-refile
              :before
              #'knavemacs/org-refile--insert-link)

  ;; capture templates
  (setq org-capture-templates
	'(
          ("n" "Work Note" entry (file+olp+datetree "~/Documents/org/journal.org" "Journal")
           "* %^{Note Title} %^G\n%U\n%?" :empty-lines-after 1)

          ("t" "Todo" entry (file+olp+datetree "~/Documents/org/journal.org" "Journal")
           "* TODO %^{Enter Task} %^G\n%?" :empty-lines-after 1)

          ("s" "Scheduled Todo" entry (file+olp "~/Documents/org/tickler.org" "Scheduled TODOs")
           "* TODO %^{Enter Scheduled Task} %?")

          ("m" "Meeting Notes" entry (file+olp+datetree "~/Documents/org/journal.org" "Journal")
           "* %t %^{Meeting Title} %^G\n** Attendees\n- [ ] %?\n** Notes\n** Action Items\n*** TODO " :empty-lines-after 1)
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
    "SPC p" "Project Commands"
    "SPC g" "Git Commands"
    "SPC t" "Tab Commands"
    "SPC h" "Help Commands"
    "SPC v" "Version Control"
    "SPC x" "Ctrl-X Commands"
    "SPC b" "Switch Buffer"
    "SPC k" "Kill Buffer"
    "SPC i" "IBuffer"))

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
            (rename-buffer "*Dired-Side*")
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
    (knavemacs/quick-window-jump))

  (eval-after-load 'dired
    '(progn
       (define-key dired-mode-map (kbd "C-<return>") 'knavemacs/window-dired-open-directory) 
       ;; q to go back to parent
       ;; i to show subdir in same buffer
       (define-key dired-mode-map (kbd "C-k") 'kill-current-buffer)
       (define-key dired-mode-map (kbd "C-o") 'knavemacs/dired-open-display-direction)
       (define-key dired-mode-map (kbd "C-i") 'dired-kill-subdir))))

;; ==================================================
;;; SECTION 5 Elisp-Built Functionality
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
;;; SECTION 6 Manually Installed Elisp Files
;; ==================================================
;;; sr-speedbar.el
;; --------------------------------------------------
(add-to-list 'load-path "~/.config/emacs/speedbar")
(require 'sr-speedbar)
(setq speedbar-use-images nil)
(setq speedbar-show-unknown-files t)

;; ==================================================
;;; SECTION 7 Global Set Keybinds
;; ==================================================
(global-set-key (kbd "M-o") #'knavemacs/quick-window-jump)
(global-set-key (kbd "M-i") #'knavemacs/window-dired-vc-root-left)
(global-set-key (kbd "M-p") #'sr-speedbar-toggle)
(global-set-key (kbd "S-TAB") #'completion-at-point)
(global-set-key (kbd "S-<iso-lefttab>") #'completion-at-point)
(global-set-key (kbd "M-g r") #'recentf)
(global-set-key (kbd "M-s g") #'grep)
(global-set-key (kbd "C-x ;") #'comment-line)
(global-set-key (kbd "RET") #'newline-and-indent)

;; ==================================================
;;; SECTION 8 {External Packages}
;; ==================================================
;;; {core} vertico packages
;; - vertico
;; - savehist
;; - marginalia
;; - orderless
;; --------------------------------------------------
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("C-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :ensure t
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
;;; {core} avy
;; --------------------------------------------------
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

;; --------------------------------------------------
;;; {core} expand-region
;; --------------------------------------------------
(use-package expand-region
  :ensure t)

;; --------------------------------------------------
;;; {core} multiple-cursors
;; --------------------------------------------------
(use-package multiple-cursors
  :ensure t)

;; --------------------------------------------------
;;; {core} surround
;; --------------------------------------------------
(use-package surround
  :ensure t)

;; --------------------------------------------------
;;; {core} ryo-modal
;; --------------------------------------------------
(use-package ryo-modal
  :ensure t
  :load-path "~/.config/emacs/modal"
  :commands ryo-modal-mode
  :bind
  ("C-z" . ryo-modal-mode) ; backup key
  ("`" . ryo-modal-mode) ; intended key
  ("C-`" . knavemacs/modal--print-backtick) ; allow printing of backtick
  :config
  (require 'knavemacs-modal-keyfuncs) ; load modal key functions

  ;; hook into existing maps using SPC as a leader
  (define-key ryo-modal-mode-map (kbd "SPC h") 'help-command)
  (define-key ryo-modal-mode-map (kbd "SPC x") ctl-x-map)
  (define-key ryo-modal-mode-map (kbd "SPC v") vc-prefix-map)
  (define-key ryo-modal-mode-map (kbd "SPC p") project-prefix-map)
  (define-key ryo-modal-mode-map (kbd "\"") surround-keymap)
  (define-key ctl-x-map (kbd "s") #'(lambda () (interactive) (if ryo-modal-mode (save-buffer) (save-some-buffers))))
  (define-key ctl-x-map (kbd "f") #'knavemacs/modal--find-file) ;; needs to be called interactively
  (define-key ctl-x-map (kbd "c") #'save-buffers-kill-terminal)
  (define-key ctl-x-map (kbd "j") #'dired-jump)

  ;; custom menus using SPC as a leader
  (ryo-modal-key
   "SPC" '(("j" switch-to-buffer) ; jump to buffer
  	   ("k" kill-current-buffer)
  	   ("i" ibuffer)
	   ("1" delete-other-windows)
	   ("2" split-window-below)
	   ("3" split-window-right)
	   ("0" delete-window)
	   ("t t" tab-line-mode)
	   ("t T" tab-bar-mode)
	   ("t j" knavemacs/tab-line-pinned-switch-to-buffer)
	   ("t r" knavemacs/tab-line-pinned-reset-buffers)
	   ("t p" knavemacs/tab-line-pinned-pin-buffer)
	   ("t u" knavemacs/tab-line-pinned-unpin-buffer)
	   ("t 1" tab-close-other)
	   ("t 2" tab-new)
	   ("t 0" tab-close)
	   ("t o" tab-next)
	   ("T p" treemacs-add-and-display-current-project)
	   ("T P" treemacs-add-and-display-current-project-exclusively)
	   ("T T" treemacs-select-window)
	   ("T d" treemacs-select-directory)
	   ("T f" treemacs-find-file)
	   ("g s" magit-status)
  	   ("o c" org-capture)
  	   ("o a" org-agenda)
  	   ("o t" knavemacs/org-quick-time-stamp-inactive)
  	   ("o l" org-store-link)))

  (ryo-modal-keys
   ("," knavemacs/modal--scroll-up-half-page)
   ("." knavemacs/modal--scroll-down-half-page)
   ("'" surround-insert)
   ("\\" ryo-modal-repeat)
   ("/" isearch-forward)
   ("?" isearch-backward)
   ("<" beginning-of-buffer)
   (">" end-of-buffer)
   ("["
    (("t"
      knavemacs/tab-line-pinned-prev-tab)))
   ("]"
    (("t"
      knavemacs/tab-line-pinned-next-tab)))
   ("{" backward-paragraph)
   ("}" forward-paragraph)
   ("a" back-to-indentation :exit t) ; append
   ("A" end-of-line :exit t) ; append to end
   ("b" backward-word) ; backword word (to beginning)
   ("B" knavemacs/modal--backward-symbol) ; backword symbol (to beginning)
   ("c" kill-ring-save) ; copy
   ("C" copy-to-buffer) ; copy to buffer
   ("d" knavemacs/modal--dwim-delete) ; delete
   ("D" kill-whole-line) ; delete line
   ("e" knavemacs/modal--increment-expression) ; forward select expression
   ("E" knavemacs/modal--decrement-expression) ; backward select expression
   ("F" avy-goto-char-timer) ; find
   ("f" avy-goto-char-in-line) ; fly
   ("g" ; _goto_ commands
    (("v"
      knavemacs/modal--jump-back-to-mark)
     ("V"
      exchange-point-and-mark)
     ("u"
      universal-argument)
     ("1"
      knavemacs/tab-line-pinned-switch-1)
     ("2"
      knavemacs/tab-line-pinned-switch-2)
     ("3"
      knavemacs/tab-line-pinned-switch-3)
     ("4"
      knavemacs/tab-line-pinned-switch-4)
     ("5"
      knavemacs/tab-line-pinned-switch-5)
     ("6"
      knavemacs/tab-line-pinned-switch-6)
     ("7"
      knavemacs/tab-line-pinned-switch-7)
     ("8"
      knavemacs/tab-line-pinned-switch-8)
     ("9"
      knavemacs/tab-line-pinned-switch-9)
     ))
   ("G" keyboard-quit) ; cancel (Ctrl-G alternative)
   ("h" backward-char) ; left
   ("H" beginning-of-line) ; all the way left
   ("i" ryo-modal-mode) ; insert
   ("I" delete-region :exit t)
   ("j" next-line) ; down
   ("J" knavemacs/modal--shift-point-bottom) ; all the way down
   ("k" previous-line) ; up
   ("K" knavemacs/modal--shift-point-top) ; all the way up
   ("l" forward-char) ; right
   ("L" end-of-line) ; all the way right
   ("M" move-to-window-line-top-bottom) ; middle
   ("m" recenter-top-bottom) ; move line to middle, top, bottom
   ("n" er/expand-region) ; expaNd regioN
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
   ("o" knavemacs/modal--open-line-below :exit t) ; open line below
   ("O" knavemacs/modal--open-line-above :exit t) ; open line above
   ("p" ; multiple points
    (("p"
      mc/mark-all-like-this :mc-all t)
     ("["
      mc/mark-previous-like-this :mc-all t)
     ("]"
      mc/mark-next-like-this :mc-all t)
     ("o"
      mc/mark-pop)))
   ("P" mc/edit-lines :mc-all t) ; multiple points per line
   ("Q" revert-buffer) ; quit all changes in buffer
   ("R" delete-region :then '(yank)) ; Replace with top kill-ring item
   ("r" knavemacs/modal--read-replacement-text) ; replace by asking
   ("s" avy-goto-word-1 :then '(set-mark-command forward-word)) ; select from word start
   ("S" avy-goto-line :then '(knavemacs/modal--set-mark-line exchange-point-and-mark)) ; select from line (or line num)
   ("t" transpose-words) ; transpose words
   ("T" transpose-lines) ; transpose lines
   ("u" undo) ; undo
   ("U" undo-redo) ; reverse undo (redo)
   ("v" knavemacs/modal--set-or-cancel-mark) ; start visual region select, or cancel visual region 
   ("V" knavemacs/modal--set-mark-line) ; visually select the line or next line
   ("w" forward-word) ; forward word
   ("W" forward-symbol) ; forward full word/symbol
   ("x" delete-char) ; delete character
   ("X" backward-delete-char-untabify) ; reverse delete character (backspace)
   ("y" yank) ; yank
   ("Y" yank-pop) ; yank from kill ring (fuzzy select)
   ("z" knavemacs/modal--delete-region-if-active :then '(zap-up-to-char) :exit t) ; zap up to char
   ("Z" knavemacs/modal--delete-region-if-active :then '(zap-to-char) :exit t)) ; zap including char

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
   ("9" "M-9")))

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
  ;;        ("C-c p f" . cape-file)
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
;;; {programming} treesitter
;; - tree-sitter
;; - tree-sitter-langs
;; --------------------------------------------------
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; --------------------------------------------------
;;; {programming} magit
;; --------------------------------------------------
(use-package magit
  :if (eq system-type 'gnu/linux)
  :ensure t)

;; --------------------------------------------------
;;; {visual} rainbow-mode
;; --------------------------------------------------
(use-package rainbow-mode
  :ensure t)

;; --------------------------------------------------
;;; {visual} treemacs
;; - treemacs
;; - treemacs-icons-dired
;; - treemacs-magit
;; - treemacs-tab-bar
;; --------------------------------------------------
(use-package treemacs
  :ensure t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-project-follow-mode t)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;; ==================================================
;;; SECTION X Modeline Configuration
;; ==================================================
(defface knavemacs/modeline-faces-modal
  '((t :foreground "#EEEEEE"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-readonly
  '((t :foreground "#7d0a36"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)


(defface knavemacs/modeline-faces-modified
  '((t :foreground "#7d0a36"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)

(defface knavemacs/modeline-faces-kmacrorec
  '((t :foreground "#7d0a36"
  	   ))
  "Default Face"
  :group 'knavemacs/mode-line-faces)
  
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
        (propertize "" 'face 'knavemacs/modeline-faces-readonly)))
  "Modeline module to provide a readonly indicator for appropriate buffers")

;; modeline module: modified indicator
(defvar-local knavemacs/modeline-modified-indicator
    '(:eval
  	  (when (buffer-modified-p)
        (propertize "" 'face 'knavemacs/modeline-faces-modified)))
  "Modeline module to provide a modified indicator for appropriate buffers")

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
        mode-name))
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

;; ------------MODELINE PREPARE VARIABLES
(dolist (construct '(knavemacs/modeline-modal-indicator
					 knavemacs/modeline-readonly-indicator
  					 knavemacs/modeline-modified-indicator
  					 knavemacs/modeline-bufname
					 knavemacs/modeline-major-mode-icon
					 knavemacs/modeline-major-mode-name
  					 knavemacs/modeline-right-display
  					 knavemacs/modeline-kmacro-indicator))
  (put construct 'risky-local-variable t)) ;; required for modeline local vars

;; ------------MODELINE FUNCTIONS
(defun knavemacs/modeline-fill-for-alignment ()
  "Modeline module to provide filler space until right-aligned items are added to modeline."
  (let ((r-length (length (concat (format-mode-line knavemacs/modeline-right-display) (format-mode-line knavemacs/modeline-kmacro-indicator)) )))
    (propertize " "
                'display `(space :align-to (- right ,r-length)))))

(defun knavemacs/return-modal-state ()
  "Returns the current viper state, or a default string if void."
  (interactive)
  (if ryo-modal-mode
	  (setq modal-mode-string "󰮢 GRAVE")
	(setq modal-mode-string " EMACS"))
  (format-mode-line 'modal-mode-string))


;; ------------MODELINE CONSTRUCTION
(setq-default mode-line-format
  			  '("%e"
  				" "
  				knavemacs/modeline-modal-indicator
  				mode-line-front-space
  				knavemacs/modeline-readonly-indicator
  				" "
  				knavemacs/modeline-modified-indicator
  				" "
  				knavemacs/modeline-bufname
  				" "
  				knavemacs/modeline-major-mode-icon
  				" "
  				knavemacs/modeline-major-mode-name
  				(:eval (knavemacs/modeline-fill-for-alignment))
  				knavemacs/modeline-right-display
  				knavemacs/modeline-kmacro-indicator))

;; ==================================================
;;; SECTION Y Final Startup Sequence
;; ==================================================
(select-frame-set-input-focus (selected-frame))
(global-auto-revert-mode 1)
(indent-tabs-mode -1)
(electric-pair-mode 1)
(recentf-mode 1)
(repeat-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(winner-mode)
(xterm-mouse-mode 1)
(file-name-shadow-mode 1) ; allows us to type a new path without having to delete the current one

(with-current-buffer (get-buffer-create "*scratch*")
  (insert (format ";;
;; ██╗  ██╗███╗   ██╗ █████╗ ██╗   ██╗███████╗███╗   ███╗ █████╗  ██████╗███████╗
;; ██║ ██╔╝████╗  ██║██╔══██╗██║   ██║██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;; █████╔╝ ██╔██╗ ██║███████║██║   ██║█████╗  ██╔████╔██║███████║██║     ███████╗
;; ██╔═██╗ ██║╚██╗██║██╔══██║╚██╗ ██╔╝██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;; ██║  ██╗██║ ╚████║██║  ██║ ╚████╔╝ ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;; ╚═╝  ╚═╝╚═╝  ╚═══╝╚═╝  ╚═╝  ╚═══╝  ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;   Loading time : %s
;;   Packages     : %s
;;
"
          	  (emacs-init-time)
          	  (number-to-string (length package-activated-list)))))
(message (emacs-init-time))
(if (eq system-type 'gnu/linux) (shell-command "notify-send 'Emacs Configuration Loaded'"))
