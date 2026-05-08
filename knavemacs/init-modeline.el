;; -*- lexical-binding: t; eval: (local-set-key (kbd "C-c C-c") #'imenu); eval: (setq imenu-generic-expression '(("Sections" "^;;; \\(.*\\)$" 1))); -*-

;; init-modeline.el - custom modeline
;; Part of Knavemacs

;;
;;; N1 faces
;;
(defface knavemacs/modeline-faces--bufname
  '((t :foreground "#3311DD"
  	   ))
  "Bufname Face"
  :group 'knavemacs/modeline-faces)


;;
;;; N2 modules
;;

;; modeline module: buffer name
(defvar-local knavemacs/modeline--bufname
  	'(:eval
  	  (propertize (buffer-name) 'face 'knavemacs/modeline-faces--bufname))
  "Modeline module to provide the buffer name.")

;;
;;; N3 define modules
;;
(dolist (construct '(
  					 knavemacs/modeline--bufname))
  (put construct 'risky-local-variable t)) ;; required for modeline local vars

;;
;;; N4 modeline construction
;;
(setq-default mode-line-format
  			  '("%e"
  				;mode-line-front-space
                                ;mode-line-modes
  				knavemacs/modeline--bufname))
;  				(:eval (knavemacs/modeline-fill-for-alignment))
;  				knavemacs/modeline-kmacro-indicator))

