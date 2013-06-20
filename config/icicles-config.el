(defun set-icicles-options ()
  (require-package 'icicles)
  (require-package 'fuzzy-match)

  (setq icicle-TAB-completion-methods '(fuzzy vanilla basic))

  (icy-mode))

(set-icicles-options)

(provide 'icicles-config)
