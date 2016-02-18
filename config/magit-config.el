(require-package 'magit)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c f") 'magit-file-popup)

;; If Emacs supports it, install magit-inotify
(when (and (> emacs-major-version 24)
           (> emacs-minor-version 4))
  (require-package 'magit-inotify)
  (add-hook 'magit-mode-hook 'magit-inotify-mode))

;; Not strictly magit, but useful and gitty

(require-package 'git-gutter+)
(require-package 'git-gutter-fringe+)
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)

(provide 'magit-config)
