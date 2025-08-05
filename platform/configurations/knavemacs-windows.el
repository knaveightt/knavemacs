;; Windows-sepecific settings and configuration
(setq org-agenda-files (list "C:\\Users\\josinski\\OneDrive - JNJ\\org" "C:\\Users\\josinski\\OneDrive - JNJ\\org\\areas"))

;; capture templates
(setq org-capture-templates
      '(
        ("n" "Work Note" entry (file+olp+datetree "C:\\Users\\josinski\\OneDrive - JNJ\\org\\journal.org" "Journal")
         "* %^{Note Title} %^G\n%U\n%?" :empty-lines-after 1)

        ("t" "Todo" entry (file+olp+datetree "C:\\Users\\josinski\\OneDrive - JNJ\\org\\journal.org" "Journal")
         "* TODO %^{Enter Task} %^G\n%?" :empty-lines-after 1)

        ("f" "Future Todo" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\tickler.org" "Future")
         "* TODO %^{Enter Scheduled Task} %?")

        ("m" "Meeting Notes" entry (file+olp+datetree "C:\\Users\\josinski\\OneDrive - JNJ\\org\\journal.org" "Journal")
         "* %t %^{Meeting Title} %^G\n** Attendance\n|Attendee|Present|\n|-|-|\n|%?\n** Notes\n** Action Items\n*** TODO " :empty-lines-after 1)
        ))

(load-theme 'modus-operandi-tinted t)

(provide 'knavemacs-platform)
