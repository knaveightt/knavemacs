;; Windows-sepecific settings and configuration
(setq org-agenda-files (list "C:\\Users\\josinski\\OneDrive - JNJ\\org" "C:\\Users\\josinski\\OneDrive - JNJ\\org\\areas"))

;; capture templates

(setq org-capture-templates
      '(
        ("p" "New Project" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\backlog.org" "Captured")
         "* PROJECT %^{Enter Project} %^G\n** TODO%^{Enter Task} %?\n" :empty-lines-after 1)

        ("t" "Quick Task" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\backlog.org" "Captured")
         "* TODO %?\nSCHEDULED: %t\n" :empty-lines-after 1)

        ("m" "Meeting Notes" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\backlog.org" "Captured")
         "* %t %^{Meeting Title} %^G\n** Attendance\n|Attendee|Present|\n|-|-|\n|%?\n** Notes\n** Action Items\n*** (Begin Todos) " :empty-lines-after 1)

	("w" "Work Notes" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\backlog.org" "Captured")
         "* %? %^G\n" :empty-lines-after 1)
        ))

(provide 'knavemacs-platform)
