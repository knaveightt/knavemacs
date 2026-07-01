;; Part of Knavemacs
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("" "○" "●" "✿" "❀" "✜" "◆" "▶"))
  (org-ellipsis " ")
  :config
  ;; a fix for color issues with org-indent mode
  (defun knavemacs/org-indent-fix-colors ()
    "Function to adjust some faces when org-bullets are used with org indent mode"
    (set-face-attribute 'org-hide nil :foreground (face-attribute 'default :background nil t))
    (set-face-attribute 'org-hide nil :background (face-attribute 'default :background nil t))
    (set-face-attribute 'org-indent nil :foreground (face-attribute 'default :background nil t))
    (set-face-attribute 'org-indent nil :background (face-attribute 'default :background nil t)))
  (advice-add 'org-indent-mode :after #'knavemacs/org-indent-fix-colors))
