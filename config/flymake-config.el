(require 'package-config)

(defun set-flymake-options ()
  (require-package 'flymake 'flymake-cursor)
  )

(set-flymake-options)

(provide 'flymake-config)
