;; Windows-sepecific settings and configuration
(setq org-agenda-files (list "C:\\Users\\josinski\\OneDrive - JNJ\\org" "C:\\Users\\josinski\\OneDrive - JNJ\\org\\areas"))

;; capture templates
(setq org-capture-templates
      '(	
	  ("t" "Todo" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "TODOs")
         "* %^{Enter Context} %^G\n** %^{Task Type|TODO|PROJECT} %?\n" :empty-lines-after 1)

	  ("q" "Quick Task" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "TODOs" "Quick Tasks")
         "* TODO %?\n" :empty-lines-after 1)

        ("f" "Future Todo" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "TODOs" "Future Tasks")
         "* TODO %?\n" :empty-lines-after 1)

        ("m" "Meeting Notes" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "Meeting Notes")
         "* %t %^{Meeting Title} %^G\n** Attendance\n|Attendee|Present|\n|-|-|\n|%?\n** Notes\n** Action Items\n*** (Begin Todos) " :empty-lines-after 1)
        ))

(provide 'knavemacs-platform)


