(require-package 'auto-complete)
(require 'auto-complete-config)

;; Initalize autocomplete
(ac-config-default)
(ac-flyspell-workaround)
(ac-linum-workaround)

(provide 'autocomplete-config)
