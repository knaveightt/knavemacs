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
    "SPC h" "Help Commands"
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

