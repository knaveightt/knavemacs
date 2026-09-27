;; ==================================================
;; org-super-agenda.el
;; Org Super Agenda - an updated view with updated 
;; functionality.
;;
;; Part of Knavemacs configuration
;;==================================================
(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode t))

(defun knavemacs/org-super-agenda-file-and-parent (item)
  "Return 'file / parent heading' for ITEM."
  (let ((marker (or (get-text-property 0 'org-marker item)
                    (get-text-property 0 'org-hd-marker item))))
    (when marker
      (with-current-buffer (marker-buffer marker)
        (save-excursion
          (goto-char marker)

          ;; File name
          (let* ((file (file-name-base
                        (buffer-file-name)))

                 ;; Immediate parent heading
                 (parent
                  (save-excursion
                    (when (org-up-heading-safe)
                      (org-get-heading t t t t)))))

            (format "%s / %s"
                    file
                    (or parent "Top Level"))))))))

(add-to-list 'org-agenda-custom-commands
      '("t" "Todos by File/Header"
         alltodo ""
         ((org-super-agenda-groups
           '((:auto-map knavemacs/org-super-agenda-file-and-parent))))))

