;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Marvin Krause"
      user-mail-address "krause.marvin@proton.me")

(defvar mk-alexandria-dir "~/Nextcloud/alexandria")


;; See 'C-h v doom-font' for documentation and more examples
(setq doom-font (font-spec :family "JetBrains Mono" :size 14 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 14))

(setq doom-theme 'doom-gruvbox)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Org & Agenda
(defvar mk-org-directory (concat mk-alexandria-dir "/org"))
(defvar mk-agenda-directory (concat mk-org-directory "/agenda/"))
(use-package! org
  :hook (org-mode . org-indent-mode)
  :bind
  (("C-c n a" . org-agenda))
  :custom
  (org-directory mk-org-directory)
  (org-log-into-drawer t)
  (org-agenda-files `(,(expand-file-name mk-agenda-directory))))

;; Zettelkasten
(defvar mk-roam-directory (concat mk-org-directory "/roam"))
(defvar mk-bibliography (concat mk-roam-directory "/bibliography"))
(defvar mk-bibliography-bibfile (concat mk-bibliography "/bibliography.bib"))

(use-package! citar
  :custom
  (citar-library-paths `(,mk-bibliography))
  (citar-bibliography `(,mk-bibliography-bibfile)))

(use-package! citar-org-roam
  :after citar org-roam
  :no-require
  :config (citar-org-roam-mode))
