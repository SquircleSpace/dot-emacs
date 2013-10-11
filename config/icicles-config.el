(require-package 'icicles)
(require-package 'fuzzy-match)

(setq icicle-TAB-completion-methods '(fuzzy vanilla basic))

(icy-mode)

(provide 'icicles-config)
