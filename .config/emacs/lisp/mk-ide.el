(use-package flycheck
  :init (global-flycheck-mode))

(use-package treemacs
  :config (treemacs-project-follow-mode)
  :bind ("C-c o d" . treemacs-select-window))

(use-package magit
  :bind ("C-x g" . magit))

(use-package eglot
  :custom (eglot-autoshutdown t))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package aggressive-indent
  :hook (prog-mode))

(use-package editorconfig)

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :hook ((clojure-mode emacs-lisp-mode) . paredit-mode))

(use-package project
  :config
  (defun project-magit-dir ()
    "Run Magit in the current project's root."
    (interactive)
    (magit-status (project-root (project-current t))))

  :custom
  (project-switch-commands '((project-find-file "Find file")
			     (project-find-regexp "Find regexp")
			     (project-find-dir "Find directory")
			     (project-magit-dir "Magit" "v")
			     (project-eshell "Eshell"))))


;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;
(use-package clojure-mode)

(use-package cider
  :hook (cider-repl-mode . paredit-mode))

(use-package flycheck-clj-kondo)

;;;;;;;;;;
;; Rust ;;
;;;;;;;;;;
(use-package rustic
  :custom
  (rustic-lsp-client 'eglot))

(provide 'mk-ide)
