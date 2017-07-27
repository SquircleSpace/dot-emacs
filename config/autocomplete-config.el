(use-package auto-complete
  :config
  (progn
    (ac-config-default)
    (ac-flyspell-workaround)
    (ac-linum-workaround)))

(provide 'autocomplete-config)
