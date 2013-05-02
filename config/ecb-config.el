(require 'package-config)

(defun set-ecb-options ()
  (require-package 'ecb)

  ; Make left click the primary mouse button
  (setq ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)

  )

(set-ecb-options)

(provide 'ecb-config)
