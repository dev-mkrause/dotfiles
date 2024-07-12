(use-package magit
  :after transient)

(use-package flymake
  :ensure nil
  :config
  (add-hook 'prog-mode-hook #'flymake-mode)
  (add-hook 'text-mode #'flymake-mode))

(use-package eglot
  :ensure nil
  :config
  (setq eglot-autoshutdown t))

(use-package eat
  :config
  (setq eat-kill-buffer-on-exit t)
  
  (use-package eat
    :after project
    :bind ([remap project-shell] . eat-project)))

(use-package buffer-env
  :config
  (add-hook 'hack-local-variables-hook #'buffer-env-update)
  (add-hook 'comint-mode-hook #'buffer-env-update))

(use-package paredit
  :config
  (dolist (h '(clojure-mode-hook cider-repl-mode-hook emacs-lisp-mode-hook scheme-mode-hook racket-mode-hook))
    (add-hook h #'paredit-mode)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package clojure-mode)
(use-package cider)
(setq cider-repl-pop-to-buffer-on-connect 'display-only)

(use-package geiser
  :config
  (setq geiser-autodoc-identifier-format "%s -> %s"))

(use-package geiser-guile
  :config
  (when (executable-find "guix")
    (add-to-list 'geiser-guile-load-path
		 (expand-file-name "~/.config/guix/current/share/guile/site/3.0"))))
