(use-package icicles
  :demand t
  :config
  (progn
    (setq icicle-TAB-completion-methods '(fuzzy vanilla basic))
    (icy-mode 1)))

(provide 'icicles-config)
