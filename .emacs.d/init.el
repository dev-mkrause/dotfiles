(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(require 'use-package)
(setq use-package-always-ensure t
      use-package-always-defer t)

(defvar alexandria-dir "~/Nextcloud/alexandria")

;; Completion Framework
(use-package vertico
  :config
  (setq vertico-cycle t)
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
  :init
  (setq completion-styles '(orderless)))


(use-package company-box
  :init
  (setq company-minimum-prefix-length 1)
  (setq company-show-quick-access t)
  (global-company-mode))


(use-package magit)


(use-package eglot
  :config
  (setq eglot-autoshutdown t))


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
