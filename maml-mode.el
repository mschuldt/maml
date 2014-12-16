(defvar maml-mode-hook nil)

(define-derived-mode maml-mode python-mode
  "maml-mode"
  )

(add-to-list 'auto-mode-alist '("\\.maml\\'" . maml-mode))
(provide 'maml-mode)
