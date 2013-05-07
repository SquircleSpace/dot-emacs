(defun set-magit-options ()

  (require-package 'magit)
  (global-set-key (kbd "C-c s") 'magit-status)

  )

(set-magit-options)

(provide 'magit-config)
