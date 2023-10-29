(require 'org)


(defcustom org-gtd-directory
  (expand-file-name "agenda/" org-directory)
  "Directory where agenda files are stored, that should be used by org-gtd.")

(defcustom org-gtd-inbox-file
  "inbox.org"
  "Name for org-gtd's inbox file.")


;; TODO org--refile-entry am ende von gtd--inbox-mapper Items automatisch in agenda.org nach TODO Tag einsortieren lassen
;; (defun gtd--refile-entry ()
;;   "Refile entrys to other files in `org-gtd-directory', except `org-gtd-inbox-file'."
;;   (interactive)
;;   (let* ((org-refile-targets (remove "inbox.org"
;; 				     (directory-files org-gtd-directory nil directory-files-no-dot-files-regexp))))
;;     ))

(defun gtd--inbox-mapper ()
  "Functions, that will get mapped over single org mode item in `org-gtd-inbox-file'."
  (org-narrow-to-element)
  (org-set-tags-command)
  (org-todo)
  (org-refile)
  (narrow-to-page))

(defun gtd-classify-inbox ()
  "Classify items in `org-gtd-inbox-file'."
  (interactive)
  (let* ((inbox-file (expand-file-name org-gtd-inbox-file org-gtd-directory))
	 (org-agenda-files '(inbox-file)))
    (find-file-other-window inbox-file)
    (org-map-entries #'gtd--inbox-mapper nil `(,inbox-file) nil)))

(provide 'org-gtd)
