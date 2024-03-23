(use-package org-contrib
  :after org
  :config
  (add-to-list 'org-modules 'org-depend)
  (add-to-list 'org-modules 'org-habit))

(defun mk-classify-entry ()
  "Classify entry at point."
  (interactive)
  (org-todo)
  (org-set-effort)
  (org-set-tags-command)
  (org-refile))

(use-package org
  :bind
  (("C-c n a" . org-agenda)
   ("C-c n c" . org-capture)
   ("C-c n w" . mk-classify-entry))
  :custom
  (org-directory "~/Dokumente/org")
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)" "WAITING(@w!)" "|" "DONE(@d)" "CANCELLED(@c)")))
  (org-log-into-drawer t)
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil))

;; Files
(setq org-directory "~/Dokumente/org")
(setq org-agenda-files (list "agenda/inbox.org" "agenda/agenda.org"
                             "agenda/notes.org" "agenda/projects.org"))

;; Capture
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "agenda/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on/ %U"))
        ("t" "Termin" entry  (file+headline "agenda/agenda.org" "Termine")
         ,(concat "* %? :termin:\n"
                  "<%<%Y-%m-%d %a %H:00>>"))
        ("n" "Note" entry  (file "agenda/notes.org")
         ,(concat "* Note (%a)\n"
                  "/Entered on/ %U\n" "\n" "%?"))
	("f" "Zettelkasten Fleeting" entry  (file "~/Dokumente/zettelkasten/fleeting.org")
         ,(concat "* %^{Zusammenfassung}\n"
		  "%?\n"
		  "- Quelle: %\n" "- Seite: %^{Seite}"))))

(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; Refile
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
	("agenda.org" :regexp . "\\Actions\\|Termine")
	("agenda.org" :level . 1)
	("agenda.org" :maxlevel . 1)
	("someday.org" :maxlevel . 1)))

(defun log-todo-next-creation-date (&rest ignore)
  "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
  (when (and (string= (org-get-todo-state) "NEXT")
             (not (org-entry-get nil "ACTIVATED")))
    (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
(add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)

;; Agenda
(setq org-agenda-custom-commands
      '(("g" "GTD"
	 ((agenda ""
		  ((org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'deadline))
		   (org-scheduled-past-days 0)
		   (org-deadline-warning-days 0)))
	  (todo "NEXT"
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'deadline))
		 (org-agenda-prefix-format "  %i %-12:c [%e] ")
		 (org-agenda-overriding-header "\nTasks\n")))
	  (todo "WAITING"
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'deadline))
		 (org-agenda-prefix-format "  %i %-12:c [%e] ")
		 (org-agenda-overriding-header "\nWarten auf...\n")))
	  (agenda nil
		  ((org-agenda-entry-types '(:deadline))
		   (org-agenda-format-date "")
		   (org-agenda-span 1)
		   (org-deadline-warning-days 7)
		   (org-agenda-overriding-header "\nDeadlines")))
	  (agenda "" ((org-agenda-overriding-header "Overdue")
		      (org-agenda-time-grid nil)
		      (org-agenda-start-on-weekday nil)
		      (org-agenda-show-all-dates nil)
		      (org-agenda-format-date "")  ;; Skip the date
		      (org-agenda-span 1)
		      (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		      (org-agenda-entry-types '(:deadline :scheduled))
		      (org-scheduled-past-days 999)
		      (org-deadline-past-days 999)
		      (org-deadline-warning-days 0)))
	  (tags-todo "inbox"
		     ((org-agenda-prefix-format "  %?-12t% s")
		      (org-agenda-overriding-header "\nInbox\n")))
	  (tags "CLOSED>=\"<today>\""
		((org-agenda-overriding-header "\nCompleted today\n")))))))

(add-hook 'org-agenda-finalize-hook #'org-agenda-find-same-or-today-or-agenda 7)

(setq org-log-done 'time)

(let ((org-agenda-files (mapcar 'file-truename 
				(file-expand-wildcards "~/Dokumente/org/agenda/*.org"))))

  ;; Save the corresponding buffers
  (defun gtd-save-org-buffers ()
    "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
    (interactive)
    (message "Saving org-agenda-files buffers...")
    (save-some-buffers t (lambda () 
			   (when (member (buffer-file-name) org-agenda-files) 
			     t)))
    (message "Saving org-agenda-files buffers... done")))

;; Add it after refile
(advice-add 'org-refile :after
            (lambda (&rest _)
              (gtd-save-org-buffers)))

;; Save org files after quitting agenda
(advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

;; Tracking working hours
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)

;; Display time in hours not in days
(setq org-duration-format (quote h:mm))

(provide 'mk-organization)
