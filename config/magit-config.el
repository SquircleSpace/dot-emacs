(use-package magit
  :bind (("C-c s" . magit-status)
         ("C-c f" . magit-file-popup)))

;; Not strictly magit, but useful and gitty
(use-package git-gutter+
  :demand t
  :config
  (global-git-gutter+-mode t))

(use-package git-gutter-fringe+
  :after git-gutter+)

(provide 'magit-config)
