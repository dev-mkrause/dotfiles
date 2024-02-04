(require 'ox-latex)

;; Zettelkasten Setup
(use-package denote
  :bind (("C-c n d n" . denote)
         ("C-c n d f" . denote-open-or-create)
         ("C-c n d d" . denote-date)
         ("C-c n d i" . denote-link-or-create)
         ("C-c n d l" . denote-find-link)
         ("C-c n d b" . denote-find-backlink)
         ("C-c n d D" . denote-org-dblock-insert-links)
         ("C-c n d R" . denote-rename-file-using-front-matter)
         ("C-c n d k" . denote-keywords-add)
         ("C-c n d K" . denote-keywords-remove))
  :custom ((denote-directory "~/Dokumente/zettelkasten")
	   (denote-known-keywords '())
	   (denote-prompts '(signature title keywords)))
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :hook ((dired-mode . denote-dired-mode)))

(use-package bibtex
  :custom
  (bibtex-dialect 'BibTeX)
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file" "Link to document file." ":")))
  (bibtex-align-at-equal-sign t))


(use-package citar
  :bind (("C-c n d c c" . citar-create-note)
         ("C-c n d c o" . citar-denote-open-note)
         ("C-c n d c d" . citar-denote-dwim)
         ("C-c n d c a" . citar-denote-add-citekey)
         ("C-c n d c x" . citar-denote-remove-citekey)
	 ("C-c n d c o" . citar-open))
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))


(use-package citar-denote
  :custom ((citar-open-always-create-notes t)
	   (citar-open-always-create-notes t)
	   (citar-denote-title-format "author-year"))

  :config (citar-denote-mode))




(use-package ebib)

(global-set-key (kbd  "C-c n d e") #'ebib)


(setq bibtex-completion-bibliography '("~/Dokumente/zettelkasten/library/library.bib"))
;;(setq bibtex-completion-notes-path "~/Dokumente/zettelkasten/library/")

(setq bibtex-completion-library-path '("~/Dokumente/zettelkasten/library/"))
(setq citar-library-paths bibtex-completion-library-path)

(setq org-cite-global-bibliography '("~/Dokumente/zettelkasten/library/library.bib"))
(setq citar-bibliography org-cite-global-bibliography)


;; Multiple LaTeX passes for bibliographies
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


;; Clean temporary files after export
(setq org-latex-logfiles-extensions
      (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
              "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
              "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
              "tex" "bcf")))


(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools)


;;;;;;;;;;;;;
;; Reading ;;
;;;;;;;;;;;;;
(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files '("~/Dokumente/org/elfeed.org")))

(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed ))

(provide 'mk-zettelkasten)
