(require 'package-config)

(defun set-autocomplete-options ()
  (require-package 'auto-complete)
  (require 'auto-complete-config)
  (ac-config-default)
  (ac-flyspell-workaround)
  )

(set-autocomplete-options)

(provide 'autocomplete-config)
