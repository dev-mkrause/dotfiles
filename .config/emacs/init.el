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
(require 'mk-organization)
(require 'mk-ide)
(require 'mk-latex)
(require 'mk-zettelkasten)

(mk-emacs-package-install 'beancount "https://github.com/beancount/beancount-mode")

(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'flymake-bean-check-enable)

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(setq lsp-completion-provider :none)
(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'corfu-lsp-setup)


(use-package go-mode)

(use-package blacken)
(use-package eglot)
(use-package numpydoc)
(use-package pyvenv)

(use-package yaml-mode)

;; Hooks
(add-hook 'python-mode-hook #'blacken-mode)
(add-hook 'python-mode-hook #'eldoc-mode)
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-mode-hook #'pyvenv-mode)
(add-hook 'python-mode-hook #'pyvenv-tracking-mode)


;;; pyvenv
;; restart python when the virtual environment changes
(add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python)

;; default to the commonly used "venv" folder for the virtual
;; environment
(customize-set-variable 'pyvenv-default-virtual-env-name "venv")

;;; python mode
(customize-set-variable 'python-indent-guess-indent-offset-verbose nil)

;;; numpydoc
(customize-set-variable 'numpydoc-insert-examples-block nil)
(customize-set-variable 'numpydoc-template-long nil)

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
(put 'narrow-to-region 'disabled nil)
