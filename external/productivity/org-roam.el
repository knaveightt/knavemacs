;; ==================================================
;; org-roam.el
;; Org Roam - notetaking system.
;;
;; Part of Knavemacs configuration
;;==================================================
(use-package org-roam
  :if knavemacs/BFlags--org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (org-roam-db-autosync-mode))
