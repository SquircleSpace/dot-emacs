(use-package flyspell-config)
(use-package python
  :config
  (add-hook 'python-mode-hook 'flyspell-prog-mode))

(provide 'python-config)
