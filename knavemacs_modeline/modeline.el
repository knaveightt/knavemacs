;; modeline.el - custom modeline
;; Part of Knavemacs

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

(defface knavemacs/modeline-faces-text
  '((t :foreground "#DDDDDD"
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
(set-face-attribute 'mode-line nil :foreground "#CCCCCC")
(set-face-attribute 'mode-line-inactive nil :background "#030303")
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)


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
  	  (propertize (buffer-name) 'face 'knavemacs/modeline-faces-text))
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
  (let ((final-mdln ""))
    (when (buffer-file-name)
      (let ((file (buffer-file-name)))
	(when (vc-backend file)
	  (when (eq (vc-backend file) 'Git)
	    (when vc-mode
	      (let* ((gbranch (elt (split-string vc-mode ":") 1))
		     (mdln (concat "  " gbranch)))
		(setq final-mdln mdln)))))))
    final-mdln))

;; ------------modeline CONSTRUCTION
(setq-default mode-line-format
  			  '("%e"
  				;mode-line-front-space
  				;knavemacs/modeline-modal-indicator
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

