;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;
(use-package tex
  :ensure auctex

  :config
  (setq-default TeX-engine 'luatex)
  (setq-default TeX-master nil)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  :custom
  ((TeX-engine 'luatex)
   (TeX-auto-save t)
   (TeX-parse-self t)

   (TeX-source-correlate-start-server t)
   (TeX-view-program-selection '((output-pdf "PDF Tools")))))

(provide 'mk-latex)
