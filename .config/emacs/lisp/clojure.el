(use-package clojure-mode)

(use-package cider
  :hook (cider-repl-mode . paredit-mode))

(use-package flycheck-clj-kondo)

(provide 'clojure)
