(require 'package)
(require 'use-package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'clojure)
(require 'zettelkasten)
(require 'org-gtd)
(require 'ide)
(require 'presentation)

;; (use-package doom-themes
;;   :config
;;   (setq  doom-themes-enable-bold t
;; 	 doom-themes-enable-italic t)
;;   (load-theme 'doom-one t)
;;   (doom-themes-org-config))
(use-package nix-mode
  :mode "\\.nix\\'")

(use-package envrc
  :bind ("C-c o e r" . envrc-reload)
  ("C-c o e a" . envrc-allow))
(setq epg-pinentry-mode 'loopback)

(setenv "GPG_AGENT_INFO" nil)

(use-package modus-themes
  :config (load-theme 'modus-vivendi-tinted t))

(use-package doom-modeline
  :config (doom-modeline-mode))

(use-package restart-emacs
  :bind ("C-h u r" . restart-emacs))

(use-package vertico
  :custom (vertico-cycle t)

  :config (vertico-mode)
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package consult
  :bind (("C-s" . consult-line)

	 :map minibuffer-local-map
	 ("C-r" . consult-history)))


(use-package marginalia
  :custom (marginalia-annotators '(marginalia-annotators-heavy
				   marginalia-annotators-light))
  :config (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :config
  (with-eval-after-load 'embark-consult
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

  (setq prefix-help-command #'embark-prefix-help-command)
  (keymap-global-set "<remap> <describe-bindings>" #'embark-bindings)
  :bind (("C-." . embark-act)))

(use-package embark-consult)

(use-package corfu
  :custom
  (corfu cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 1)

  :config
  (corfu-popupinfo-mode)
  (eldoc-add-command #'corfu-insert)

  :bind (:map corfu-map
	      ("M-p" . corfu-popupinfo-scroll-down)
	      ("M-n" . corfu-popupinfo-scroll-up)
	      ("M-d" . corfu-popupinfo-toggle))
  :init (global-corfu-mode))

(use-package cape
  :config
  ;; Add useful defaults completion sources from cape
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  ;; Silence the pcomplete capf, no errors or messages!
  ;; Important for corfu
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files '("~/Dokumente/org/elfeed.org")))

(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed ))


;; Emacs general settings
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

(recentf-mode)
(global-set-key (kbd "C-c f r") #'recentf)

(setq use-short-answers t)

(set-face-attribute 'default nil :font "Fira Code" :height 120)

(setq vc-follow-symlinks t)

(setq native-comp-async-report-warnings-errors 'silent)


(defun open-user-init-file ()
  "Open users init file defined by 'user-init-file'."
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-h u c") #'open-user-init-file)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :hook ((clojure-mode emacs-lisp-mode) . paredit-mode))

(use-package vterm
  :bind ("<f12>". vterm))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package org
  :bind
  (("C-c n a" . org-agenda)
   ("C-c n c" . org-capture))
  :custom
  (org-directory "~/Dokumente/org")
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROJECT(p)" "WAITING(w)"
				 "DELEGATED(l)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c)")))
  (org-agenda-files '("~/Dokumente/org/agenda/agenda.org"))
  (org-log-into-drawer t)
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil))

(setq org-capture-templates
      '(("i" "Inbox" entry (file "~/Dokumente/org/agenda/inbox.org")
         "* %?\n  %i\n")))
