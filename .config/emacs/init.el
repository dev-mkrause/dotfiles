;;; init.el --- My personal emacs configuration      -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Marvin Krause

;; Author: Marvin Krause

;;; Code:
(require 'package)
(require 'use-package)
(require 'project)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(defun mk/package-install (package)
  (unless (package-installed-p package)
    (package-install package)))

(defmacro mk/keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(load-theme 'modus-vivendi :no-confirm)
(bind-key (kbd "<f5>") #'modus-themes-toggle)

(add-to-list 'default-frame-alist
             '(font . "Iosevka Nerd Font-13"))

(setq display-line-numbers-type t
      use-short-answers t
      vc-follow-symlinks t
      enable-recursive-minibuffers t)

(recentf-mode)
(pixel-scroll-precision-mode)
(repeat-mode 1)
(savehist-mode 1)

(defvar --backup-directory (expand-file-name "backups" user-emacs-directory))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))

(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      )

(mk/package-install 'restart-emacs)

(mk/package-install 'doom-modeline)
(doom-modeline-mode)

(mk/package-install 'which-key)
(which-key-mode)


;;;;;;;;;;;;;;;;;
;; Tree Sitter ;;
;;;;;;;;;;;;;;;;;
;; (mk/package-install 'tree-sitter-langs)
;; (require 'tree-sitter-langs)
;; (tree-sitter-hl-mode)
;; (global-tree-sitter-mode)

;;;;;;;;;;;;;;;;;;
;; Coding & IDE ;;
;;;;;;;;;;;;;;;;;;
(mk/package-install 'magit)

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'text-mode #'flymake-mode)

(setq eglot-autoshutdown t)

;;;;;;;;;;;;;;;;
;; Lisp Modes ;;
;;;;;;;;;;;;;;;;
(mk/package-install 'clojure-mode)
(mk/package-install 'cider)
(setq cider-repl-pop-to-buffer-on-connect 'display-only)

(add-to-list 'display-buffer-alist
	     '("cider-repl.*" display-buffer-in-direction
	       (direction . bottom)
	       (window . root) (window-height . 0.3)))

(mk/package-install 'paredit)
(dolist (h '(clojure-mode-hook cider-repl-mode-hook emacs-lisp-mode-hook scheme-mode-hook racket-mode-hook))
 	(add-hook h #'paredit-mode))

(mk/package-install 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;
;; vterm setup ;;
;;;;;;;;;;;;;;;;;
(mk/package-install 'vterm)

(defun project-vterm ()
  "Open vterm in the current project root or prompt for one if no project is active."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root    (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer  (bound-and-true-p display-comint-buffer-action))
      (vterm))))

(setq vterm-kill-buffer-on-exit t)
(add-hook 'vterm-mode-hook (lambda () (setq confirm-kill-processes nil))) ;; Don't prompt for running processes for open vterms when closing emacs

(add-to-list 'display-buffer-alist
	     '(".*-vterm.*" display-buffer-in-direction
	       (direction . bottom)
	       (window . root) (window-height . 0.3)))

(add-to-list 'project-switch-commands '(project-vterm "Open VTerm"))
(mk/keybind project-prefix-map
  "t" #'project-vterm)


;;;;;;;;;;;;;;;;;;;;;;;
;; Completion system ;;
;;;;;;;;;;;;;;;;;;;;;;;
(mk/package-install 'vertico)
(setq vertico-cycle t)


(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(mk/package-install 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(mk/package-install 'embark)
(mk/package-install 'embark-consult)
(mk/package-install 'consult)
(mk/package-install 'marginalia)

(mk/package-install 'corfu)
(mk/package-install 'nerd-icons-corfu)

(require 'corfu)
(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
(setq corfu-auto t
      corfu-auto-prefix 2
      corfu-cycle t)

(global-corfu-mode)
(vertico-mode)
(marginalia-mode)

(mk/keybind global-map
  "C-." #'embark-act)


;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;
(mk/package-install 'org-contrib)

(require 'org)
(add-to-list 'org-modules 'org-depend)
(setq org-directory "~/Dokumente/org")
(setq org-agenda-files (directory-files "~/Dokumente/org/" t "\\.org$"))

(setq org-log-into-drawer t
      org-cycle-separator-lines 0 ;; Don't fold headlines if content is only empty lines.
      org-ellipsis "⤵"
      org-log-done 'time
      org-todo-keywords '((sequence "TODO(t!)" "NEXT(n!)" "PROJECT(p!)" "WAITING(w@)" "|" "DONE(d@)" "CANCELLED(c@)")))

(setq org-capture-templates
      '(("t" "Personal todo" entry
         (file "inbox.org")
         "* TODO %?\n%i" :prepend t)
        ("n" "Personal notes" entry
         (file+headline "inbox.org" "Notes")
         "* %u %?\n%i\n%a" :prepend t)
        ("j" "Journal" entry
         (file+olp+datetree "journal.org")
         "* %U %?\n%i\n%a" :prepend t)
        ("p" "Templates for projects")))

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

(defun mk/org-insert-trigger ()
  "Automatically insert chain-find-next trigger when entry becomes NEXT"
  (cond ((or (equal org-state "NEXT") (equal org-state "WAITING"))
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((not (member org-state org-done-keywords))
         (org-delete-property "TRIGGER"))))

;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda () 
			 (when (member (buffer-file-name) org-agenda-files) 
			   t)))
  (message "Saving org-agenda-files buffers... done"))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)")
	("todo.org" :maxlevel . 1)
	("someday.org" :level . 1)))

;; Add it after refile
(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))

(add-hook 'org-after-todo-state-change-hook 'mk/org-insert-trigger)
(add-hook 'org-capture-mode-hook 'delete-other-windows)


;;;;;;;;;;;;;;;;;;
;; Zettelkasten ;;
;;;;;;;;;;;;;;;;;;
(mk/package-install 'denote)
(mk/package-install 'citar)
(mk/package-install 'citar-denote)
(mk/package-install 'pdf-tools)

(setq denote-directory "~/Dokumente/zettelkasten"
      denote-known-keywords '()
      denote-prompts '(title keywords signature))
(add-hook 'dired-mode-hook #'denote-dired-mode)


(setq bibtex-completion-bibliography `(,(expand-file-name "references.bib" denote-directory)))
(setq citar-bibliography bibtex-completion-bibliography)

(setq org-noter-always-create-frame nil)

(setq citar-library-paths `(,(expand-file-name "library/" denote-directory))
      citar-open-always-create-notes t)

(setq citar-open-always-create-notes t
       citar-denote-title-format "author-year")
(citar-denote-mode)


;;;;;;;;;;;;;
;; Keymaps ;;
;;;;;;;;;;;;;
(defvar-keymap mk/prefix-zettelkasten-map
  :doc "Prefix map for zettelkasten."
  :name "Zettelkasten"
  "d"     #'denote
  "f"     #'denote-open-or-create
  "i"     #'denote-link
  "o"     #'denote-find-link
  "O"     #'citar-open
  "b"     #'denote-find-backlink
  "a"     #'denote-keywords-add
  "A"     #'denote-keywords-remove
  "c"     #'citar-create-note
  "k"     #'citar-denote-add-citekey
  "K"     #'citar-denote-remove-citekey)

(defvar-keymap mk/prefix-open-map
  :doc "Prefix map for opening my stuff."
  :name "Open"
  "t" #'vterm
  "r" #'recentf)

(defvar-keymap mk/prefix-notes-map
  :doc "Prefix keymap for notes and organization."
  :name "Notes"
  "a" #'org-agenda
  "n" #'org-capture
  "d" mk/prefix-zettelkasten-map)

(defvar-keymap mk/prefix-search-map
  :doc "Prefix map for search functions."
  :name "Search"
  "s" #'consult-line
  "d" #'consult-fd
  "r" #'consult-ripgrep)

(defvar-keymap mk/prefix-quit-map
  :doc "Prefix keymap for quitting and restarting."
  :name "Quit"
  "r" #'restart-emacs
  "R" #'restart-emacs-start-new-emacs
  "f" #'delete-frame)

(mk/keybind global-map
  "C-z" nil ;; Remove suspend frame
  "C-x C-z" nil
  "C-c n" mk/prefix-notes-map
  "C-c s" mk/prefix-search-map
  "C-c o" mk/prefix-open-map
  "C-c q" mk/prefix-quit-map)

(mk/keybind minibuffer-local-map
  "M-a" #'marginalia-cycle)

;;;;;;;;;;;;;
;; Gimmics ;;
;;;;;;;;;;;;;
(mk/package-install 'nerd-icons)
(mk/package-install 'nerd-icons-dired)
(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
