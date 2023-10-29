(use-package magit
  :bind ("C-x g" . magit))

(use-package eglot)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(provide 'ide)
