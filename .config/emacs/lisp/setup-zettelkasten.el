(use-package denote
  :config
  (setq denote-directory "~/Dokumente/zettelkasten"
	denote-known-keywords '()
	denote-prompts '(title keywords signature))

  
  (defun mk/dired-zettelkasten ()
    "Open zettelkasten's 'denote-directory' in dired"
    (interactive)
    (delete-other-windows)
    (denote-directory))

  
  (add-hook 'dired-mode-hook #'denote-dired-mode))
(use-package citar)
(use-package citar-denote
  :config
  (citar-denote-mode))

(use-package pdf-tools)

(setq bibtex-completion-bibliography `(,(expand-file-name "references.bib" denote-directory)))
(setq bibtex-dialect 'biblatex)
(setq citar-bibliography bibtex-completion-bibliography)

(setq org-noter-always-create-frame nil)

(setq citar-library-paths `(,(expand-file-name "library/" denote-directory))
      citar-open-always-create-notes t)

(setq citar-open-always-create-notes t
      citar-denote-title-format nil)

(use-package nov
  :config
  (setq nov-text-width 80
	visual-fill-column-center-text t)
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))


(use-package elfeed
  :config
  (use-package elfeed-org
    :config
    (setq rmh-elfeed-org-files '("~/Dokumente/elfeed.org"))
    (elfeed-org)))




