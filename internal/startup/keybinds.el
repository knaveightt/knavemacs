;; ==================================================
;; keybinds.el
;; Keybinds Initialization - global key map 
;; modifications
;;
;; Part of Knavemacs configuration
;;==================================================

(global-set-key (kbd "S-TAB") #'completion-at-point)
(global-set-key (kbd "S-<iso-lefttab>") #'completion-at-point)
(global-set-key (kbd "M-g r") #'recentf)
(global-set-key (kbd "M-s g") #'grep)
(global-set-key (kbd "C-x ;") #'comment-line)
(global-set-key (kbd "C-x =") #'text-scale-adjust)
(global-set-key (kbd "C-c d") #'global-eldoc-mode)
(global-set-key (kbd "C-c m") #'menu-bar-open)
(global-set-key (kbd "C-c C-m") #'menu-bar-mode)
(global-set-key (kbd "RET") #'newline-and-indent)

