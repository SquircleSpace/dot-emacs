(use-package magit
  :bind (("C-c s" . magit-status)
         ("C-c f" . magit-file-popup))
  :config
  (setf magit-log-arguments '("--graph" "--color" "--decorate" "-n256")))

;; Not strictly magit, but useful and gitty
(use-package git-gutter+
  :demand t
  :config
  (global-git-gutter+-mode t))

(use-package git-gutter-fringe+
  :after git-gutter+)

(use-package vc
  :ensure nil
  :config
  (setf vc-git-resolve-conflicts nil))

(provide 'magit-config)
