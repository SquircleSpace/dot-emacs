(defun set-ido-options ()
  (require-package 'ido 'ido-ubiquitous 'ido-yes-or-no)
  (require 'ido)
  (require 'ido-ubiquitous)
  (require 'ido-yes-or-no)

  ; Turn on ido
  (ido-mode)

  ; Use flex matching
  (setq ido-enable-flex-matching t)

  ; More ido is more better
  (ido-everywhere)
  (ido-ubiquitous-mode)
  (ido-yes-or-no-mode)

  )

(set-ido-options)

(provide 'ido-config)
