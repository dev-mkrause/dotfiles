
(use-package org
  :bind
  (("C-c n a" . org-agenda)
   ("C-c n c" . org-capture))
  :custom
  (org-directory "~/Dokumente/org")
  ;; (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)" "WAITING(w)"
  ;; 				 "DELEGATED(l)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-log-into-drawer t)
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil))


(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/Dokumente/org/agenda/inbox.org")
         "* %?\n  %i\n")))


;; Tracking working hours
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)


;; Getting things done
(setq org-gtd-update-ack "3.0.0")
(use-package org-gtd
  :after org

  :init
  (require 'org-gtd)
  (require 'org-gtd-archive)
  (require 'org-gtd-projects)
  (require 'org-gtd-delegate)

  :custom
  (org-gtd-directory "~/Dokumente/org/agenda/")

  :config
  (org-gtd-mode)
  
  :bind
  (("C-c d c" . org-gtd-capture)
   ("C-c d f" . org-gtd-engage-grouped-by-context)
   ("C-c d e" . org-gtd-engage)
   ("C-c d p" . org-gtd-process-inbox)
   :map org-gtd-clarify-map
   ("C-c c" . org-gtd-organize)))

(provide 'mk-organization)
