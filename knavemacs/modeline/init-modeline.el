;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c C-c") #'imenu); eval: (setq imenu-generic-expression '(("Sections" "^;;; \\(.*\\)$" 1))); -*-

;; init-modeline.el - custom modeline
;; Part of Knavemacs

;;
;;; N0 color definitions
;;
(defvar knavemacs/modeline-colors--text "#3311DD")
(defvar knavemacs/modeline-colors--indicator-bg "#444444")
(defvar knavemacs/modeline-colors--indicator-fg "#3311DD")
(defvar knavemacs/modeline-colors--indicator-insert-bg "#11DD33")
(defvar knavemacs/modeline-colors--indicator-insert-fg "#111111")
(defvar knavemacs/modeline-colors--indicator-motion-bg "#999999")
(defvar knavemacs/modeline-colors--indicator-motion-fg "#3311DD")
(defvar knavemacs/modeline-colors--modified "#FDB91F")
(defvar knavemacs/modeline-colors--unmodified "#48DD10")
(defvar knavemacs/modeline-colors--readonly "#7D0A3D")

;;
;;; N1 faces
;;
;; modal indicator
(defface knavemacs/modeline-faces--modal-indicator
  `((t :foreground ,knavemacs/modeline-colors--indicator-fg
       :background ,knavemacs/modeline-colors--indicator-bg
       :weight bold
  	   ))
  "Modal Indicator (Default)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--modal-indicator-insert
  `((t :foreground ,knavemacs/modeline-colors--indicator-insert-fg
       :background ,knavemacs/modeline-colors--indicator-insert-bg
       :weight bold
  	   ))
  "Modal Indicator (Insert)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--modal-indicator-motion
  `((t :foreground ,knavemacs/modeline-colors--indicator-motion-fg
       :background ,knavemacs/modeline-colors--indicator-motion-bg
  	   ))
  "Modal Indicator (Motion)"
  :group 'knavemacs/modeline-faces)

;; Bufname
(defface knavemacs/modeline-faces--bufname
  `((t :foreground ,knavemacs/modeline-colors--text
  	   ))
  "Bufname Face"
  :group 'knavemacs/modeline-faces)

;; Major mode name
(defface knavemacs/modeline-faces--major-mode-name
  `((t :foreground ,knavemacs/modeline-colors--text
  	   ))
  "Major Mode Name Face"
  :group 'knavemacs/modeline-faces)

;; Right Display
(defface knavemacs/modeline-faces--right-display
  `((t :foreground ,knavemacs/modeline-colors--text
       :background ,knavemacs/modeline-colors--indicator-bg
  	   ))
  "Right Display (Default)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--right-display-insert-mode
  `((t :foreground ,knavemacs/modeline-colors--indicator-insert-fg
       :background ,knavemacs/modeline-colors--indicator-insert-bg
  	   ))
  "Right Display (Insert)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--right-display-motion-mode
  `((t :foreground ,knavemacs/modeline-colors--text
  	   ))
  "Right Display (Motion)"
  :group 'knavemacs/modeline-faces)

;; Left Separator
(defface knavemacs/modeline-faces--left-separator
  `((t :foreground ,knavemacs/modeline-colors--indicator-bg
  	   ))
  "Left Separator (Default)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--left-separator-insert-mode
  `((t :foreground ,knavemacs/modeline-colors--indicator-insert-bg
  	   ))
  "Left Separator (Insert)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--left-separator-motion-mode
  `((t :foreground ,knavemacs/modeline-colors--indicator-motion-bg
  	   ))
  "Left Separator (Motion)"
  :group 'knavemacs/modeline-faces)

;; Right Separator
(defface knavemacs/modeline-faces--right-separator
  `((t :foreground ,knavemacs/modeline-colors--indicator-bg
  	   ))
  "Right Separator (Default)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--right-separator-insert-mode
  `((t :foreground ,knavemacs/modeline-colors--indicator-insert-bg
  	   ))
  "Right Separator (Insert)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--right-separator-motion-mode
  `((t :foreground ,(face-background 'mode-line)
       :background ,(face-background 'mode-line)
  	   ))
  "Right Separator (Motion)"
  :group 'knavemacs/modeline-faces)

;; modified indicator
(defface knavemacs/modeline-faces--modified
  `((t :foreground ,knavemacs/modeline-colors--modified
  	   ))
  "Modified indicator"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--unmodified
  `((t :foreground ,knavemacs/modeline-colors--unmodified
  	   ))
  "Unmodified Indicator"
  :group 'knavemacs/modeline-faces)

;; readonly indicator
(defface knavemacs/modeline-faces--readonly
  `((t :foreground ,knavemacs/modeline-colors--readonly
  	   ))
  "Readonly indicator"
  :group 'knavemacs/modeline-faces)

;;
;;; N2 define modules
;;
;; modeline module: modal indicator
(defvar-local knavemacs/modeline--modal-indicator
  	'(:eval
  	  (when (mode-line-window-selected-p)
            (if (multistate-insert-state-p)
                (propertize (knavemacs/return-modal-state) 'face 'knavemacs/modeline-faces--modal-indicator-insert)
              (if (multistate-motion-state-p)
                  (propertize (knavemacs/return-modal-state) 'face 'knavemacs/modeline-faces--modal-indicator-motion)
  	        (propertize (knavemacs/return-modal-state) 'face 'knavemacs/modeline-faces--modal-indicator)))))
  "Modeline module to show modal / Emacs state indicator.")

;; modeline module: left separator
(defvar-local knavemacs/modeline--left-separator
  	'(:eval
  	  (when (mode-line-window-selected-p)
            (if (multistate-insert-state-p)
                (propertize "" 'face 'knavemacs/modeline-faces--left-separator-insert-mode)
              (if (multistate-motion-state-p)
                  (propertize "" 'face 'knavemacs/modeline-faces--left-separator-motion-mode)
  	        (propertize "" 'face 'knavemacs/modeline-faces--left-separator)))))
  "Modeline module to show a simple separator.")

;; modeline module: right separator
(defvar-local knavemacs/modeline--right-separator
  	'(:eval
  	  (when (mode-line-window-selected-p)
            (if (multistate-insert-state-p)
                (propertize "" 'face 'knavemacs/modeline-faces--right-separator-insert-mode)
              (if (multistate-motion-state-p)
                  (propertize "" 'face 'knavemacs/modeline-faces--right-separator-motion-mode)
  	        (propertize "" 'face 'knavemacs/modeline-faces--right-separator)))))
  "Modeline module to show a simple separator.")

;; modeline module: major mode icon
(defvar-local knavemacs/modeline--major-mode-icon
    '(:eval
	  (when (mode-line-window-selected-p)
        (nerd-icons-icon-for-mode major-mode)))
  "Modeline module to provide an icon based on the major mode.")

;; modeline module: buffer name 
(defvar-local knavemacs/modeline--bufname
  	'(:eval
  	  (propertize (buffer-name) 'face 'knavemacs/modeline-faces--bufname))
  "Modeline module to provide the buffer name.")

;; modeline module: major mode name
(defvar-local knavemacs/modeline--major-mode-name
    '(:eval
      (when (mode-line-window-selected-p)
        (propertize (concat (format-mode-line mode-name) " ") 'face 'knavemacs/modeline-faces--major-mode-name)))
  "Modeline module to provide the major mode name.")

;; modeline module: modified indicator
(defvar-local knavemacs/modeline--modified-indicator
    '(:eval
      (if (buffer-modified-p)
          (propertize " " 'face 'knavemacs/modeline-faces--modified)
        (propertize " " 'face 'knavemacs/modeline-faces--unmodified))))

;; modeline module: readonly indicator
(defvar-local knavemacs/modeline--readonly-indicator
    '(:eval
  	  (when buffer-read-only
        (propertize " " 'face 'knavemacs/modeline-faces--readonly)))
  "Modeline module to provide a readonly indicator for appropriate buffers")

;; modeline module: right display
(defvar-local knavemacs/modeline--right-display
  	'(:eval
          (when (mode-line-window-selected-p)
            (if (multistate-insert-state-p)
  	        (propertize " L%l:C%c [%p] " 'face 'knavemacs/modeline-faces--right-display-insert-mode)
              (if (multistate-motion-state-p)
                  (propertize " L%l:C%c [%p] " 'face 'knavemacs/modeline-faces--right-display-motion-mode)
                (propertize " L%l:C%c [%p] " 'face 'knavemacs/modeline-faces--right-display)))))
  "Modeline module ot provide minimal modeline info aligned right.")

;;
;;; N3 modeline functions
;;
(defun knavemacs/modeline-fill-for-alignment ()
  "Modeline module to provide filler space until right-aligned items are added to modeline."
  (let ((r-length (length (concat
                           (format-mode-line knavemacs/modeline--major-mode-icon)
                           (format-mode-line knavemacs/modeline--major-mode-name)
                           (format-mode-line knavemacs/modeline--right-separator)
                           (format-mode-line knavemacs/modeline--right-display)
                           " "
                           ))))
    (propertize " "
                'display `(space :align-to (- right ,r-length)))))

(defun knavemacs/return-modal-state ()
  "Returns the current viper state, or a default string if void."
  (interactive)
  (if knavemacs/BFlags--nerd-icons
    (progn
      (if (multistate-emacs-state-p) (setq modal-mode-string "  Emacs "))
      (if (multistate-motion-state-p) (setq modal-mode-string "  Motion "))
      (if (multistate-insert-state-p) (setq modal-mode-string "  Insert "))
      (if (multistate-normal-state-p) (setq modal-mode-string " 󰈙 Normal ")))
    (if (multistate-emacs-state-p) (setq modal-mode-string " [E]macs "))
    (if (multistate-motion-state-p) (setq modal-mode-string " [M]otion "))
    (if (multistate-insert-state-p) (setq modal-mode-string " [I]nsert "))
    (if (multistate-normal-state-p) (setq modal-mode-string " [N]ormal ")))
  (format-mode-line 'modal-mode-string))

;;
;;; N4 initialize modules
;;
(dolist (construct '(
  					 knavemacs/modeline--modal-indicator
                                         knavemacs/modeline--left-separator
                                         knavemacs/modeline--right-separator
                                         knavemacs/modeline--major-mode-icon
                                         knavemacs/modeline--major-mode-name
  					 knavemacs/modeline--bufname
                                         knavemacs/modeline--modified-indicator
                                         knavemacs/modeline--readonly-indicator
                                         knavemacs/modeline--right-display))
  (put construct 'risky-local-variable t)) ;; required for modeline local vars

;;
;;; n5 modeline construction
;;
(setq-default mode-line-format
  			  '("%e"
  				;mode-line-front-space
                                ;mode-line-modes
  				knavemacs/modeline--modal-indicator
                                knavemacs/modeline--left-separator
  				knavemacs/modeline--bufname
                                knavemacs/modeline--modified-indicator
                                knavemacs/modeline--readonly-indicator
  				(:eval (knavemacs/modeline-fill-for-alignment))
                                knavemacs/modeline--major-mode-icon
                                " "
                                knavemacs/modeline--major-mode-name
                                knavemacs/modeline--right-separator
                                knavemacs/modeline--right-display))
;  				knavemacs/modeline-kmacro-indicator))

