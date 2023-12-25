(require 'package)
(require 'use-package)

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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(setq use-package-always-ensure t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


(require 'mk-essentials)
(require 'mk-keymaps)
(mk-emacs-keybind global-map
  "<insert>" nil
  "C-x C-z" nil
  "C-x C-c" nil
  "<f2>" nil
  "M-SPC" nil
  "C-x C-c C-c" #'save-buffers-kill-emacs
  "C-z" mk-prefix-map
  "<f2>" mk-prefix-map
  "M-SPC" mk-prefix-map
  "M-o" #'delete-blank-lines)

(require 'mk-organization)
(require 'mk-ide)
(require 'mk-latex)
(require 'zettelkasten)


;;;;;;;;;;;;;;;;;;;;
;; Spell checking ;;
;;;;;;;;;;;;;;;;;;;;
(use-package jinx
  :config
  (setq jinx-languages "en_US de_DE")
  
  (require 'vertico-multiform)

  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))
  (vertico-multiform-mode 1)

  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))


;;;;;;;;;;;
;; Icons ;;
;;;;;;;;;;;
(use-package nerd-icons)
(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Encryption and GPG ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(setq epg-pinentry-mode 'loopback)

(setenv "GPG_AGENT_INFO" nil)
