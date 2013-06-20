(defun set-icicles-options ()
  (require-package 'icicles)
  (require-package 'fuzzy-match)
  (require 'icicles)

  (setq icicle-TAB-completion-methods '(fuzzy vanilla basic))

  (icy-mode)
  )

(set-icicles-options)

(provide 'icicles-config)
