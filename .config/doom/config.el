;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-font (font-spec :family "Iosevka" :size 16))

(setq doom-theme 'doom-zenburn)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dokumente/org/")

(after! org
  (setq org-log-into-drawer t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-track-ordered-property-with-tag t
        org-agenda-dim-blocked-tasks t)

  (setq org-todo-keywords '((sequence "TODO(t)" "PROJ(p)" "NEXT(n)" "WAIT(w@/!)" "DELEGATED(D@/@)" "|" "DONE(d)" "CANCELLED(c@)")
                            (sequence "[ ](T)" "[-](S)" "[?](W@/!)" "|" "[X](D)"))))

;; Guix Setup
(after! yasnippet
  (add-to-list 'yas-snippet-dirs "~/dev/guix/etc/snippets/yas"))

;; Zettelkasten Setup
(use-package! denote
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

(use-package! citar
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package! citar-denote
  :custom (citar-open-always-create-notes t)
  :config (citar-denote-mode))

(map!
 :map 'override-global-map
 (:prefix ("C-c n d" . "Zettelkasten")

 :desc "Create note"                    "n"     #'denote
 :desc "Open or create note"            "f"     #'denote-open-or-create
 :desc "Create note by date"            "d"     #'denote-date
 :desc "Insert link and create note"    "i"     #'denote-link-or-create
 :desc "Open linked note"               "l"     #'denote-find-link
 :desc "Open backlink"                  "b"     #'denote-find-backlink
 :desc "Insert dynamic block"           "D"     #'denote-org-dblock-insert-links
 :desc "Rename file after front matter" "R"     #'denote-rename-file-using-front-matter
 :desc "Add a keyword"                  "k"     #'denote-keywords-add
 :desc "Remove a keyword"               "K"     #'denote-keywords-remove

 (:prefix ("c" . "Citations")
          :desc "Create literature note"        "c"     #'citar-create-note
          :desc "Open literature note"          "o"     #'citar-denote-open-note
          :desc "Citar dwim"                    "d"     #'citar-denote-dwim
          :desc "Add citekey"                   "a"     #'citar-denote-add-citekey
          :desc "Remove citekey"                "x"     #'citar-denote-remove-citekey)))

(setq! bibtex-completion-bibliography '("~/Dokumente/zettelkasten/library/library.bib"))
;;(setq! bibtex-completion-notes-path "~/Dokumente/zettelkasten/library/")

(setq! bibtex-completion-library-path '("~/Dokumente/zettelkasten/library/"))
(setq! citar-library-paths bibtex-completion-library-path)

(setq! org-cite-global-bibliography '("~/Dokumente/zettelkasten/library/library.bib"))
(setq! citar-bibliography org-cite-global-bibliography)

;; Org Export Settings
(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil))

;; LaTeX PDF Export settings
(use-package! ox-latex
  :custom
  ;; Multiple LaTeX passes for bibliographies
  ;; (org-latex-pdf-process
  ;;  '("pdflatex -interaction nonstopmode -output-directory %o %f"
  ;;    "bibtex %b"
  ;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf" ))))

;; epub export
(use-package! ox-epub)

(use-package! nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-save-place-file (concat doom-cache-dir "nov-places")))



;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
