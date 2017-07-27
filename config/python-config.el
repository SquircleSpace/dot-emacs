(use-package flyspell-config
  :ensure nil)
(use-package python
  :config
  (add-hook 'python-mode-hook 'flyspell-prog-mode))

(provide 'python-config)
