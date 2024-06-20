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

(setq user-full-name "Marvin Krause")
(setq user-mail-address "public@mkrause.org")

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

;; (setq use-package-always-ensure nil)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(mk/package-install 'ef-themes)
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'ef-dream :no-confirm)
(bind-key (kbd "<f5>") #'modus-themes-toggle)

(use-package doom-themes
  :ensure t
  :defer
  :init
  (defun my/doom-theme-settings (theme &rest args)
    "Additional face settings for doom themes"
    (if (eq theme 'doom-rouge)
          (progn
            (setq window-divider-default-right-width 2
                  window-divider-default-bottom-width 2
                  window-divider-default-places t)
            (message "Turned on window dividers")
            (window-divider-mode 1))
        (window-divider-mode -1)
        (message "Turned off window dividers"))
    (when (string-match-p "^doom-" (symbol-name theme))
      ;; Window dividers
      (let ((class '((class color) (min-colors 256))))
        (dolist (face-spec
                 '((aw-leading-char-face (:height 2.0 :foreground unspecified :inherit mode-line-emphasis)
                    ace-window)
                   (aw-background-face (:inherit default :weight normal) ace-window)
                   (outline-1        (:height 1.25) outline)
                   (outline-2        (:height 1.20) outline)
                   (outline-3        (:height 1.16) outline)
                   (outline-4        (:height 1.12) outline)
                   ;; (tab-bar            (:background "black" :height 1.0 :foreground "white")
                   ;;  tab-bar)
                   ;; (tab-bar-tab
                   ;;  (:bold t :height 1.10 :foreground nil :inherit mode-line-emphasis)
                   ;;  tab-bar)
                   ;; (tab-bar-tab-inactive
                   ;;  (:inherit 'mode-line-inactive :height 1.10 :background "black")
                   ;;  tab-bar)
                   ))
          (cl-destructuring-bind (face spec library) face-spec
            (if (featurep library)
                (custom-set-faces `(,face ((,class ,@spec))))
              (with-eval-after-load library
                (when (string-match-p "^doom-" (symbol-name theme))
                  (custom-set-faces `(,face ((,class ,@spec)))))))))
        ;; (when (eq theme 'doom-rouge)
        ;;   (custom-set-faces `(hl-line ((,class :background "#1f2a3f")))))
        )))

  (advice-add 'load-theme :before #'my/doom-theme-settings)

  :config
  (doom-themes-org-config)
  (use-package doom-rouge-theme
    :config
    (setq doom-rouge-padded-modeline nil
          doom-rouge-brighter-comments t
          doom-rouge-brighter-tabs t))

  (use-package doom-iosvkem-theme
    :disabled
    ;; :custom-face
    ;; (default ((t (:background "#061229"))))
    :config
    (setq doom-Iosvkem-brighter-comments nil
          doom-Iosvkem-comment-bg nil
          doom-Iosvkem-brighter-modeline nil)))

(use-package custom
  :ensure nil
  :commands my/toggle-theme
  :config
  (setq custom-theme-directory (expand-file-name "lisp" user-emacs-directory))

  (defun my/toggle-theme (theme)
    "Swap color themes. With prefix arg, don't disable the
currently loaded theme first."
    (interactive
     (list
      (intern (completing-read "Load theme: "
                               (cons "user" (mapcar #'symbol-name
                                                    (custom-available-themes)))
                                     nil t))))
    (unless current-prefix-arg
      (mapc #'disable-theme custom-enabled-themes))
    (load-theme theme t)))

(use-package dashboard
  :ensure t
  :init (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-show-shortcuts nil
        dashboard-center-content t
        dashboard-items '((recents  . 15))))

(use-package spacious-padding
  :ensure t
  :defer
  :config
  (setq spacious-padding-widths
      '( :internal-border-width 16
         :header-line-width 4
         :mode-line-width 2
         :tab-width 2
         :right-divider-width 24
         :scroll-bar-width 8)))

(add-to-list 'default-frame-alist
             '(font . "Iosevka Nerd Font-13"))

(setq display-line-numbers-type t
      use-short-answers t
      vc-follow-symlinks t
      enable-recursive-minibuffers t
      save-interprogram-paste-before-kill t)

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


(use-package which-key
  :ensure t
  :defer 10
  :bind
  (:map help-map
   ("h" . which-key-show-major-mode))
  :init
  (setq which-key-sort-order #'which-key-description-order 
        which-key-idle-delay 0.8
        which-key-idle-secondary-delay 0.1
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 0
        which-key-max-display-columns nil
        which-key-min-display-lines 8
        which-key-side-window-slot -10
        which-key-show-transient-maps nil)
  :config
  (push '(("^[0-9-]\\|kp-[0-9]\\|kp-subtract\\|C-u$" . nil) . ignore)
      which-key-replacement-alist)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom)
  (add-hook 'which-key-init-buffer-hook
            (lambda () (setq-local line-spacing 3)))

  (advice-add 'which-key-mode :after
              (lambda (_arg)
                (when (featurep 'embark)
                  (setq prefix-help-command
                        #'embark-prefix-help-command))))
  
  (which-key-mode +1)
  :diminish "")

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
(mk/package-install 'magit-todos)
(eval-after-load 'magit magit-todos-mode)

(mk/package-install 'hl-todo)
(hl-todo-mode)

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'text-mode #'flymake-mode)

(setq eglot-autoshutdown t)

(mk/package-install 'buffer-env)
(add-hook 'hack-local-variables-hook #'buffer-env-update)
(add-hook 'comint-mode-hook #'buffer-env-update)

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

;;;;;;;;;
;; Nix ;;
;;;;;;;;;
(mk/package-install 'nix-mode)
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;;;;;;;;;;
;; Guix ;;
;;;;;;;;;;
(mk/package-install 'geiser)
(mk/package-install 'geiser-guile)
(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "~/dev/guix")
  (add-to-list 'geiser-guile-load-path "~/dev/nonguix"))

;; Assuming the Guix checkout is in ~/dev/guix.
;; Yasnippet configuration
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/dev/guix/etc/snippets/yas"))
;; Tempel configuration
(with-eval-after-load 'tempel
   ;; Ensure tempel-path is a list -- it may also be a string.
   (unless (listp 'tempel-path)
     (setq tempel-path (list tempel-path)))
   (add-to-list 'tempel-path "~/dev/guix/etc/snippets/tempel/*"))

(load-file "~/dev/guix/etc/copyright.el")
(setq copyright-names-regexp
      (format "%s <%s>" user-full-name user-mail-address))

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


;;;;;;;;;;;;;
;; Extras  ;;
;;;;;;;;;;;;;
(mk/package-install 'csv-mode)
(add-hook 'csv-mode-hook 'csv-guess-set-separator)

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
;; (mk/package-install 'marginalia)
(use-package marginalia
  :ensure t
  :after setup-minibuffer
  :init (marginalia-mode 1)
  :bind (:map vertico-map
         ("M-]" . marginalia-cycle))
  :config
  (pcase-dolist (`(,regexp . ,category)
                 '(("\\burl\\b" . url)
                   ("\\bHistory\\b" . history)
                   ("\\bdefinitions?\\b" . xref-location)
                   ("\\bxref\\b" . xref-location)))
    (setf (alist-get regexp marginalia-prompt-categories
                     nil nil #'equal)
          category)))

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
(mk/package-install 'ox-hugo)

(require 'org)
(add-to-list 'org-modules 'org-depend)
(setq org-directory "~/Dokumente/org")
(setq org-agenda-files (directory-files "~/Dokumente/org/" t "\\.org$"))

(setq diary-date-forms diary-european-date-forms) ;; Use european format for diary type entries

(with-eval-after-load 'ox
  (require 'ox-hugo))

(setq org-log-into-drawer t
      org-cycle-separator-lines 0 ;; Don't fold headlines if content is only empty lines.
      org-ellipsis " ▾"
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

(defun mk/org-classify ()
  "Perform a list of actions on items in inbox."
  (interactive)
  (with-current-buffer (find-file "~/Dokumente/org/inbox.org")
    (dolist (headline (org-element-map (org-element-parse-buffer) 'headline
			(lambda (elt) elt)))
	    (progn
	      (goto-char (org-element-property :begin headline))
	      (org-todo)
	      (org-set-tags-command)
	      (org-refile)))))

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
(setq bibtex-dialect 'biblatex)
(setq citar-bibliography bibtex-completion-bibliography)

(setq org-noter-always-create-frame nil)

(setq citar-library-paths `(,(expand-file-name "library/" denote-directory))
      citar-open-always-create-notes t)

(setq citar-open-always-create-notes t
       citar-denote-title-format nil)
(citar-denote-mode)

(mk/package-install 'dired-preview)
(require 'dired-preview)
(dired-preview-global-mode)

(mk/package-install 'nov)
(setq nov-text-width 80
      visual-fill-column-center-text t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(mk/package-install 'elfeed)
(mk/package-install 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files '("~/Dokumente/elfeed.org"))

;; Dired
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook #'toggle-truncate-lines)
(add-hook 'dired-mode-hook (lambda () #'dired-omit-mode))

(defun mk/dired-zettelkasten ()
  "Open zettelkasten's 'denote-directory' in dired"
  (interactive)
  (delete-other-windows)
  (dired denote-directory))

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
  "z"     #'mk/dired-zettelkasten
  "b"     #'denote-find-backlink
  "a"     #'denote-keywords-add
  "A"     #'denote-keywords-remove
  "c"     #'citar-create-note
  "k"     #'citar-denote-add-citekey
  "K"     #'citar-denote-remove-citekey)

(defvar-keymap mk/prefix-organization-map
  :doc "Prefix map for organization stuff."
  :name "Organization"
  "c"    #'mk/org-classify)

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
  "d" mk/prefix-zettelkasten-map
  "o" mk/prefix-organization-map)

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
