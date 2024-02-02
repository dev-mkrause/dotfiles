;;; mk-essentials.el --- Essentials for my Emacs configuration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Marvin Krause

;; Author: Marvin Krause

(defmacro mk-emacs-keybind (keymap &rest definitions)
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


(defun mk-emacs-package-install (package &optional method)
  "Install PACKAGE with optional METHOD.

If METHOD is nil or the `builtin' symbol, PACKAGE is not
installed as it is considered part of Emacs.

If METHOD is a string, it must be a URL pointing to the version
controlled repository of PACKAGE.  Installation is done with
`package-vc-install'.

If METHOD is a quoted list, it must have a form accepted by
`package-vc-install' such as:

\\='(denote :url \"https://git.sr.ht/~protesilaos/denote\" :branch \"main\")

If METHOD is any other non-nil value, install PACKAGE using
`package-install'."
  (unless (or (eq method 'builtin) (null method))
    (unless (package-installed-p package)
      (when (or (stringp method) (listp method))
        (package-vc-install method))
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install package))))

(defmacro mk-emacs-package (package &rest body)
  "Require PACKAGE with BODY configurations.

PACKAGE is an unquoted symbol that is passed to `require'.  It
thus conforms with `featurep'.

BODY consists of ordinary Lisp expressions.  There are,
nevertheless, two unquoted plists that are treated specially:

1. (:install METHOD)
2. (:delay NUMBER)

These plists can be anywhere in BODY and are not part of its
final expansion.

The :install property is the argument passed to
`mk-emacs-package-install' and has the meaning of METHOD
described therein.

The :delay property makes the evaluation of PACKAGE with the
expanded BODY happen with `run-with-timer'.

Also see `mk-emacs-configure'."
  (declare (indent 1))
  (unless (memq package mk-emacs-omit-packages)
    (let (install delay)
      (dolist (element body)
        (when (plistp element)
          (pcase (car element)
            (:install (setq install (cdr element)
                            body (delq element body)))
            (:delay (setq delay (cadr element)
                          body (delq element body))))))
      (let ((common `(,(when install
                         `(mk-emacs-package-install ',package ,@install))
                      (require ',package)
                      ,@body
		      (message "Emacs loaded package: %s" ',package)
                      )))
        (cond
         ((featurep package)
          `(progn ,@body))
         (delay
          `(run-with-timer ,delay nil (lambda () ,@(delq nil common))))
         (t
          `(progn ,@(delq nil common))))))))


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

(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t)

(setq modus-themes-completions
      '((matches . (extrabold underline))
        (selection . (semibold italic))))

(setq modus-themes-org-blocks 'gray-background)

(setq modus-themes-headings
      '((1 . (variable-pitch 1.5))
        (2 . (1.3))
        (agenda-date . (1.3))
        (agenda-structure . (variable-pitch light 1.8))
        (t . (1.1))))

(load-theme 'modus-vivendi :no-confirm)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

(setq-default org-startup-indented t
              org-pretty-entities t
              org-hide-emphasis-markers t
              org-startup-with-inline-images t
              org-image-actual-width '(300))

(use-package org-modern
  :hook
  (org-mode . global-org-modern-mode)
  :custom
  (org-modern-keyword nil)
  (org-modern-checkbox nil)
  (org-modern-table nil))

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

(use-package pulsar
  :config
  (pulsar-global-mode))

(use-package doom-modeline
  :config (doom-modeline-mode))


(use-package restart-emacs
  :bind ("C-h u r" . restart-emacs))


(use-package vterm
  :bind ("<f12>". vterm))


(use-package which-key
  :custom
  (which-key-separator "  ")
  (which-key-prefix-prefix "... ")
  (which-key-max-display-columns 3)
  (which-key-idle-delay 0.5)
  (which-key-idle-secondary-delay 0.25)
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40)

  :config
  (which-key-mode 1))


;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;
(use-package vertico
  :custom (vertico-cycle t)

  :config
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (vertico-mode))

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

;;;;;;;;;;;
;; Modes ;;
;;;;;;;;;;;
(global-hl-line-mode 1)
(winner-mode 1)
(recentf-mode 1)
(pixel-scroll-precision-mode 1)
(repeat-mode 1)

;;;;;;;;;;;;;;
;; Settings ;;
;;;;;;;;;;;;;;
(setq inhibit-splash-screen t)

(setq use-short-answers t)

(set-face-attribute 'default nil :font "Fira Code" :height 120)

(setq vc-follow-symlinks t)

(setq native-comp-async-report-warnings-errors 'silent
      native-compile-prune-cache t)

(provide 'mk-essentials)
