;;; setup-ide.el --- IDE functionality and programming setup  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Marvin Krause

;; Author: Marvin Krause <public@mkrause.org>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(use-package magit
  :after transient)

(use-package flymake
  :ensure nil
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)
  (mk/keybind flymake-mode-map
    "M-n" #'flymake-goto-next-error
    "M-p" #'flymake-goto-prev-error
    "C-c ! p" #'flymake-show-project-diagnostics
    "C-c ! !" #'flymake-show-buffer-diagnostics)0
  (add-hook 'prog-mode-hook #'flymake-mode)
  (add-hook 'text-mode #'flymake-mode))

(use-package compile
  :ensure nil
  :config
  (mk/keybind prog-mode-map
    "<f11>" #'compile
    "<f12>" #'recompile))

(use-package eglot
  :ensure nil
  :config
  (setq eglot-autoshutdown t))

(use-package tempel)

(use-package eat
  :config
  (setq eat-kill-buffer-on-exit t)

  (mk/keybind project-prefix-map
    [remap project-shell] #'eat-project))

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
    ;; (add-to-list 'geiser-guile-load-path
    ;; 		 (expand-file-name "~/.config/guix/current/share/guile/site/3.0"))
    (load-file "~/dev/guix/etc/copyright.el")
    (setq copyright-names-regexp
	  (format "%s <%s>" user-full-name user-mail-address))
    (with-eval-after-load 'geiser-guile
      (add-to-list 'geiser-guile-load-path "~/dev/guix"))

    ;; Yasnippet configuration
    (with-eval-after-load 'yasnippet
      (add-to-list 'yas-snippet-dirs "~/dev/guix/etc/snippets/yas"))
    ;; Tempel configuration
    (with-eval-after-load 'tempel
      ;; Ensure tempel-path is a list -- it may also be a string.
      (unless (listp 'tempel-path)
	(setq tempel-path (list tempel-path)))
      (add-to-list 'tempel-path "~/dev/guix/etc/snippets/tempel/*"))))

;;; setup-ide.el ends here
