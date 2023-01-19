(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

(defvar alexandria-dir "~/Nextcloud/alexandria")

;; Completion Framework
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package consult
  :bind
  (("C-c s" . #'consult-line)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-," . #'embark-act)
   ("C-." . #'embark-act-all)))

(use-package embark-consult)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package company-box
  :custom
  (company-minimum-prefix-length 1)
  (company-show-quick-access t)
  :init
  (global-company-mode))


(use-package magit)


;; Manage yadm dotfiles with magit
(require 'tramp)
(add-to-list 'tramp-methods
 '("yadm"
   (tramp-login-program "yadm")
   (tramp-login-args (("enter")))
   (tramp-login-env (("SHELL") ("/bin/sh")))
   (tramp-remote-shell "/bin/sh")
   (tramp-remote-shell-args ("-c"))))
(global-set-key (kbd "C-c C-d d") #'(lambda ()
				      (interactive)
				      (magit-status "/yadm::")))


(use-package eglot
  :hook (python-mode . eglot-ensure)
  (scala-mode . eglot-ensure)
  :custom
  (setq eglot-autoshutdown t))


;; Python
(use-package python-mode
  :hook
  (python-mode . eldoc-mode)
  :custom (python-indent-guess-indent-offset-verbose nil))

(use-package pyvenv
  :hook
  (python-mode)
  :init
  (add-hook 'pyvenv-post-activate-hooks #'pyvenv-restart-python))

(use-package poetry
  :bind (:map python-mode-map
	      ("C-c C-p" . poetry))
  :hook
  (python-mode . poetry-tracking-mode))

(use-package blacken
  :hook
  (python-mode))

(use-package numpydoc
  :bind (:map python-mode-map
	      ("C-c C-n" . numpydoc-generate)))


(use-package ace-window
  :bind ("M-o" . #'ace-window))


(use-package which-key
  :init
  (which-key-mode))


;; Look and feel
(use-package all-the-icons)
(use-package doom-modeline
  :init
  (doom-modeline-mode))

(use-package doom-themes
  :init
  (load-theme 'doom-gruvbox t))

;; Line numbers
(setq display-line-numbers 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Backups
(setq make-backup-files nil)



(setq custom-file "~/.emacs.d/custom.el")
