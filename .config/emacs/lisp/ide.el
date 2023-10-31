(use-package magit
  :bind ("C-x g" . magit))

(use-package eglot)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package treemacs
  :bind ("C-c o d" . treemacs-select-window))

(provide 'ide)
