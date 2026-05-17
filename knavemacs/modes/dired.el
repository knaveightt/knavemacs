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

