;;; init.el --- My personal emacs config             -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Marvin Krause

;; Author: Marvin Krause <public@mkrause.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:
(require 'use-package)

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(elpaca-wait)

(setq use-package-enable-imenu-support t)
(setq use-package-always-ensure t)

(use-package emacs :ensure nil
  :config
  (blink-cursor-mode -1)
  (auto-insert-mode))

(defun mk/package-install (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun mk/remap-command (old-command new-command)
  "Remap command from 'old-command' to 'new-command'"
  (global-set-key [remap old-command] new-command))

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

(use-package restart-emacs)

(use-package olivetti)

(use-package logos
  :after olivetti
  :config
  (add-hook 'nov-mode-hook (lambda ()
			     (setq olivetti-body-width 80
				   olivetti-minimum-body-width 60)
			     (olivetti-mode)))
  (setq logos-olivetti t))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)

  (setq doom-rouge-padded-modeline nil
        doom-rouge-brighter-comments t
        doom-rouge-brighter-tabs t)
  
  (load-theme 'modus-operandi t)
  (doom-themes-org-config))

(custom-set-faces
 '(default ((t (:family "Iosevka Comfy"
                        :slant normal :weight normal
                        :height 130 :width normal)))))

(use-package doom-modeline
  :config
  (doom-modeline-mode))

(use-package pulsar
  :config
  (pulsar-global-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package spacious-padding
  :config
  (setq spacious-padding-widths
	'( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
	`( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode 1)

  ;; Set a key binding if you need to toggle spacious padding.
  (define-key global-map (kbd "<f8>") #'spacious-padding-mode))

(use-package which-key
  :config (which-key-mode))

(setq epa-file-name-regexp "\\.\\(gpg\\|\\asc\\)\\(~\\|\\.~[0-9]+~\\)?\\'")
(epa-file-name-regexp-update)

(setq dictionary-server "dict.org")

;;;;;;;;;;;;;;;;;;
;; Coding & IDE ;;
;;;;;;;;;;;;;;;;;;
(load (expand-file-name "lisp/setup-ide" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;
;; Completion system ;;
;;;;;;;;;;;;;;;;;;;;;;;
(use-package vertico
 :config
  (setq vertico-cycle t)
  (vertico-mode))

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion)))))

(use-package embark
  :config
  (mk/keybind global-map
    "C-." #'embark-act))

(use-package embark-consult)

(use-package consult
  :config
  (setq consult-fd-args '((if
     (executable-find "fdfind" 'remote)
     "fdfind" "fd")
 "--full-path --color=never --hidden"))
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   
--smart-case --no-heading --with-filename --line-number --search-zip --hidden"))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package corfu
  :config
  (setq corfu-auto t
    	corfu-auto-prefix 2
    	corfu-cycle t)
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;;;;;;;;;;;;;
;; Personal ;;
;;;;;;;;;;;;;;
(load (expand-file-name "lisp/setup-personal" user-emacs-directory))

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;
(load (expand-file-name "lisp/setup-dired" user-emacs-directory))

;;;;;;;;;;;;;;
;; Org Mode ;;
;;;;;;;;;;;;;;
(load (expand-file-name "lisp/setup-org" user-emacs-directory))


;;;;;;;;;;;;;;;;;;
;; Zettelkasten ;;
;;;;;;;;;;;;;;;;;;
(load (expand-file-name "lisp/setup-zettelkasten" user-emacs-directory))

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

(defvar-keymap mk/prefix-open-map
  :doc "Prefix map for opening my stuff."
  :name "Open"
  "t" #'eat
  "e" #'eshell
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
  "o" #'occur
  "d" #'consult-fd
  "D" #'dictionary-search
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
(use-package nerd-icons)
(use-package nerd-icons-dired
  :config
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

;;; init.el ends here
