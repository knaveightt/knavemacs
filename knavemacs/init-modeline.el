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
(defvar knavemacs/modeline-colors--indicator-insert-fg "#000000")

;;
;;; N1 faces
;;
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

(defface knavemacs/modeline-faces--bufname
  `((t :foreground ,knavemacs/modeline-colors--text
  	   ))
  "Bufname Face"
  :group 'knavemacs/modeline-faces)

(defface knavemacs/modeline-faces--right-display
  `((t :foreground ,knavemacs/modeline-colors--text
  	   ))
  "Right Display (Default)"
  :group 'knavemacs/modeline-faces)
(defface knavemacs/modeline-faces--right-display-insert-mode
  `((t :foreground ,knavemacs/modeline-colors--indicator-insert-fg
       :background ,knavemacs/modeline-colors--indicator-insert-bg
  	   ))
  "Right Display (Insert)"
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
  	      (propertize (knavemacs/return-modal-state) 'face 'knavemacs/modeline-faces--modal-indicator))))
  "Modeline module to show modal / Emacs state indicator.")

;; modeline module: buffer name
(defvar-local knavemacs/modeline--bufname
  	'(:eval
  	  (propertize (buffer-name) 'face 'knavemacs/modeline-faces--bufname))
  "Modeline module to provide the buffer name.")

;; modeline module: right display
(defvar-local knavemacs/modeline--right-display
  	'(:eval
          (when (mode-line-window-selected-p)
  	    " L%l:C%c [%p]"))
  "Modeline module ot provide minimal modeline info aligned right.")

;;
;;; N3 initialize modules
;;
(dolist (construct '(
  					 knavemacs/modeline--modal-indicator
  					 knavemacs/modeline--bufname
                                         knavemacs/modeline--right-display))
  (put construct 'risky-local-variable t)) ;; required for modeline local vars

;;
;;; N4 modeline functions
;;
(defun knavemacs/modeline-fill-for-alignment ()
  "Modeline module to provide filler space until right-aligned items are added to modeline."
  (let ((r-length (length (concat
                           "   "
                           (format-mode-line knavemacs/modeline--right-display)
                           ))))
    (propertize " "
                'display `(space :align-to (- right ,r-length))
                'face 'knavemacs/modeline-faces--right-display)))

(defun knavemacs/return-modal-state ()
  "Returns the current viper state, or a default string if void."
  (interactive)
  (if (multistate-emacs-state-p) (setq modal-mode-string "  Emacs "))
  (if (multistate-motion-state-p) (setq modal-mode-string "  Motion "))
  (if (multistate-insert-state-p) (setq modal-mode-string "  Insert "))
  (if (multistate-normal-state-p) (setq modal-mode-string " 󰈙 Normal "))
  (format-mode-line 'modal-mode-string))

;;
;;; n5 modeline construction
;;
(setq-default mode-line-format
  			  '("%e"
  				;mode-line-front-space
                                ;mode-line-modes
  				knavemacs/modeline--modal-indicator
  				knavemacs/modeline--bufname
  				(:eval (knavemacs/modeline-fill-for-alignment))
                                knavemacs/modeline--right-display))
;  				knavemacs/modeline-kmacro-indicator))

