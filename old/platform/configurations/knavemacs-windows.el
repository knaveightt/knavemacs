;; Windows-sepecific settings and configuration
(setq org-agenda-files (list "C:\\Users\\josinski\\OneDrive - JNJ\\org" "C:\\Users\\josinski\\OneDrive - JNJ\\org\\areas"))

;; capture templates
(setq org-capture-templates
      '(
	
	("p" "New Project" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "TODOs")
         "* PROJECT %^{Enter Project} %^G\n** TODO %^{Enter First Task} %?\n" :empty-lines-after 1)

	("t" "Quick Task" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "TODOs" "Quick Tasks")
         "* TODO %?\n" :empty-lines-after 1)

        ("f" "Future Todo" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "TODOs" "Future Tasks")
         "* TODO %?\n" :empty-lines-after 1)

        ("m" "Meeting Notes" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "Meeting Notes")
         "* %t %^{Meeting Title} %^G\n** Attendance\n|Attendee|Present|\n|-|-|\n|%?\n** Notes\n** Action Items\n*** (Begin Todos) " :empty-lines-after 1)
	
	("w" "Work Notes" entry (file+olp "C:\\Users\\josinski\\OneDrive - JNJ\\org\\todos.org" "Work Notes")
         "* %? %^G\n" :empty-lines-after 1)
        ))

(provide 'knavemacs-platform)
