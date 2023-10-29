;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Emacs
(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 16))

(setq doom-theme 'modus-vivendi)
(setq display-line-numbers-type t)

(dolist (h '(clojure-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook))
        (add-hook h #'paredit-mode))

;; Org Mode
(add-to-list 'org-modules 'org-depend)
(setq org-directory "~/Dokumente/org")

(after! org
  (setq org-log-into-drawer t
        org-log-done 'time
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)" "WAITING(@w!)" "|" "DONE(@d)" "CANCELLED(@c)")))

  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file "inbox.org")
           "* TODO %?\n%i" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Notes")
           "* %u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)
          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
          ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
          ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))))

(setq org-agenda-custom-commands
      '(("g" "GTD"
         ((agenda ""
		  ((org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-span 1)
                   (org-agenda-start-day "today")
		   (org-deadline-warning-days 0)))
	  (todo "NEXT"
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'deadline))
		 (org-agenda-prefix-format "%(concat \"[ \"(car (split-string (org-format-outline-path (org-get-outline-path)) \"/\")) \" ]\")% ")
		 (org-agenda-overriding-header "\nTasks\n")))
	  (todo "WAITING"
		((org-agenda-skip-function
		  '(org-agenda-skip-entry-if 'deadline))
		 (org-agenda-prefix-format "%b")
		 (org-agenda-overriding-header "\nWarten auf...\n")))
	  (agenda nil
		  ((org-agenda-entry-types '(:deadline))
		   (org-agenda-format-date "")
		   (org-agenda-span 1)
		   (org-deadline-warning-days 14)
		   (org-agenda-overriding-header "\nDeadlines")))
	  (tags-todo "inbox"
		     ((org-agenda-prefix-format "  %?-12t% s")
		      (org-agenda-overriding-header "\nInbox\n")))))))

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

(defun mk/org-insert-trigger ()
  "Automatically insert chain-find-next trigger when entry becomes NEXT"
  (cond ((equal org-state "NEXT")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((not (member org-state org-done-keywords))
         (org-delete-property "TRIGGER"))))

(add-hook 'org-after-todo-state-change-hook 'mk/org-insert-trigger)

;; Zettelkasten
(setq! denote-directory "~/Dokumente/org/zettelkasten"
       denote-known-keywords '()
       denote-prompts '(title keywords signature))
(add-hook! dired-mode-hook #'denote-dired-mode)

(setq! bibtex-completion-bibliography `(,(expand-file-name "references.bib" denote-directory)))
(setq! citar-bibliography bibtex-completion-bibliography)

(setq! org-noter-always-create-frame nil)

;; (define-key doom-leader-notes-map (kbd "d") nil)
(map! :leader
      :prefix ("n" . "notes")
      "d" nil
      (:prefix ("d" . "zettelkasten")
       :desc            "denote"                "d"     #'denote
       :desc            "Open note or create"   "f"     #'denote-open-or-create
       :desc            "Insert link"           "i"     #'denote-link
       :desc            "Find link"             "o"     #'denote-find-link
       :desc            "Citar open"            "O"     #'citar-open
       :desc            "Find backlink"         "b"     #'denote-find-backlink
       :desc            "Add keyword"           "a"     #'denote-keywords-add
       :desc            "Remove keyword"        "A"     #'denote-keywords-remove
       :desc            "Citar note"            "c"     #'citar-create-note
       :desc            "Citar add key"         "k"     #'citar-denote-add-citekey
       :desc            "Citar remove key"      "K"     #'citar-denote-remove-citekey))

;; (setq! bibtex-completion-library-path '("/path/to/library/path/")
;;       bibtex-completion-notes-path "/path/to/your/notes/")

(setq! citar-library-paths `(,(expand-file-name "library/" denote-directory))
       citar-open-always-create-notes t)

(map! :map doom-leader-notes-map
      (:prefix "r"
               "A"       'citar-org-roam-ref-add))

(setq! citar-open-always-create-notes t
       citar-denote-title-format "author-year")
(citar-denote-mode)

;; Modes
(repeat-mode 1)
