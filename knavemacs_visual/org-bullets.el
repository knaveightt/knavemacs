;; org-bullets.el - fancy org-mode bullets
;; Part of Knavemacs
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
  (org-ellipsis " ⤵"))
