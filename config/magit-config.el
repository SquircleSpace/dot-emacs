(defun set-magit-options ()

  (require-package 'magit)
  (global-set-key (kbd "C-c s") 'magit-status)

  ;; If Emacs supports it, install magit-inotify
  (when ((and (> emacs-major-version 24)
              (> emacs-minor-version 4)))
    (require-package 'magit-inotify)
    (add-hook 'magit-mode-hook 'magit-inotify-mode))

  )

(set-magit-options)

(provide 'magit-config)
