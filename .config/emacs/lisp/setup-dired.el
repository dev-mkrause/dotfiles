(use-package dired
  :ensure nil
  :config
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'toggle-truncate-lines)
  (add-hook 'dired-mode-hook (lambda () #'dired-omit-mode))

  (use-package dirvish
    :after dired
    :config (dirvish-override-dired-mode)))
