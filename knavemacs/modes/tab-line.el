;; I use the tab-line as a way to "pin" buffers for quick
;; visiting, I have functions to pin / unpin buffers to the
;; tab line, as well as to cycle in a way that allows me
;; to quickly access the buffers that have been pinned to the
;; tab line. I can also call a function that allows me to
;; switch to a buffer that is represented on the tab line
;; only. 
(use-package tab-line
  :ensure t
  :bind (("C-c t t" . tab-line-mode)
         ("C-c t j" . knavemacs/tab-line-pinned-switch-to-buffer)
         ("C-c t r" . knavemacs/tab-line-pinned-reset-buffers)
         ("C-c t p" . knavemacs/tab-line-pinned-pin-buffer)
         ("C-c t u" . knavemacs/tab-line-pinned-unpin-buffer))
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
