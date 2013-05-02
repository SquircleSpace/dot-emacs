(require 'package-config)

(defun set-ido-options ()
  (require-package 'ido)

  ; Turn on ido
  (ido-mode)

  ; Use flex matching
  (setq ido-enable-flex-matching t)

  ; More ido is more better
  (ido-everywhere)

  )

(set-ido-options)

(provide 'ido-config)
