;; org-bullets.el - fancy org-mode bullets
;; Part of Knavemacs
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis " ⤵")
  :config
  ;; a fix for color issues with org-indent mode
  (defun knavemacs/org-indent-fix-colors ()
    "Function to adjust some faces when org-bullets are used with org indent mode"
    (set-face-attribute 'org-hide nil :background "#181a26" :foreground "#181a26")
    (set-face-attribute 'org-indent nil :background "#181a26" :foreground "#181a26"))
  (advice-add 'org-indent-mode :after #'knavemacs/org-indent-fix-colors))
