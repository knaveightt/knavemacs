;; ==================================================
;; org.el
;; Org-specific configuration
;;
;; Part of Knavemacs configuration
;;==================================================
(use-package org
  :bind (("C-c o c" . org-capture)
         ("C-c o a" . org-agenda)
         ("C-c o t" . knavemacs/org-quick-time-stamp-inactive)
         ("C-c o l" . org-store-link))
  :config
  (setq org-agenda-files (list "~/Documents/org" "~/Documents/org/areas"))
  (setq org-agenda-todo-list-sublevels nil) ;; only want to see top level TODOs in global list
  (setq org-stuck-projects '("+TODO=\"PROJECT\"" ("TODO" "FOLLOWUP")))
  (setq org-refile-targets '((org-agenda-files :level . 1)))
  (setq org-id-link-to-org-use-id t)
  (setq org-todo-keywords
	'((sequence "BACKLOG(b)" "TODO(t)" "NEXT(n)" "PROJECT(p)" "FOLLOWUP(w@)" "|" "DONE(d!)" "CANCELLED(c)")))
  (setq org-todo-keyword-faces
	'(("BACKLOG" . org-warning)
          ("TODO" . org-todo)
          ("NEXT" . org-todo)
          ("PROJECT" . org-drawer)
          ("FOLLOWUP" . org-macro)
          ("DONE" . org-done)
          ("CANCELLED" . org-property-value)
          ))

  ;; custom agenda views
  (setq org-agenda-custom-commands
	'(
	  ("d" "Todo Planning"
	   (
	    (agenda ""
		    ((org-deadline-warning-days 7)
		     (org-agenda-overriding-header "Scheduled TODOs")))
	    (tags "+TODO=\"TODO\"-SCHEDULED={.+}"
		  ((org-agenda-overriding-header "Unscheduled Work")))
		(tags "+SCHEDULED<\"<today>\"-TODO=\"DONE\""
		  ((org-agenda-overriding-header "Late Work")))
	    (stuck "" ((org-agenda-overriding-header "Stuck Projects")))
	    (tags "+TODO=\"FOLLOWUP\"-SCHEDULED={.+}"
		  ((org-agenda-overriding-header "Floating Follow-Ups")))
	    ))
          ))

  ;; org function for printing out a quick timestamp
  (defun knavemacs/org-quick-time-stamp-inactive ()
    "Insert an inactive time stamp of the current time without user prompt"
    (interactive)
    (let ((current-prefix-arg '(16)))
      (call-interactively 'org-time-stamp-inactive))
    (insert " "))

  ;; org variable and function for creating subject area files I use in this set
  (defvar knavemacs/org-subject-file-string
    (concat
     "#+TITLE: XYZ\n"
     "#+AUTHOR: John Osinski\n"
     "#+DATE: [2000-01-01 Sat 12:01]\n"
     "#+TAGS: XYZ\n\n"
     "* Knowledge\n"
     "* Task Library\n"
     "* Meeting Notes\n"))

  (defun knavemacs/org-create-area-file (subject)
    "Create a file named SUBJECT.org and write 'Hello SUBJECT' into it."
    (interactive "sEnter Subject: ")
    (let* ((filename (concat subject ".org"))
           (content (replace-regexp-in-string "XYZ" subject knavemacs/org-subject-file-string)))
      (with-temp-file filename
        (insert content))
      (message "Successfully wrote to %s" filename)))
  ;; (mapcar #'knavemacs/org-create-area-file '("TEST1" "TEST2" "TEST3"))

  ;; capture templates
  (setq org-capture-templates
	'(
          ("p" "New Project" entry (file+olp "~/Documents/org/todos.org" "TODOs")
           "* PROJECT %^{Enter Project} %^G\n** TODO%^{Enter Task} %?\n" :empty-lines-after 1)

          ("t" "Quick Task" entry (file+olp "~/Documents/org/todos.org" "TODOs" "Quick Tasks")
           "* TODO %?\nSCHEDULED: %t\n" :empty-lines-after 1)

          ("f" "Future Todo" entry (file+olp "~/Documents/org/todos.org" "TODOs" "Future Tasks")
           "* TODO %?\n" :empty-lines-after 1)

          ("m" "Meeting Notes" entry (file+olp "~/Documents/org/todos.org" "Meeting Notes")
           "* %t %^{Meeting Title} %^G\n** Attendance\n|Attendee|Present|\n|-|-|\n|%?\n** Notes\n** Action Items\n*** (Begin Todos) " :empty-lines-after 1)

	      ("w" "Work Notes" entry (file+olp "~/Documents/org/todos.org" "Work Notes")
           "* %? %^G\n" :empty-lines-after 1)
          )))
