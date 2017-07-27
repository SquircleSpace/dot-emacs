(use-package haskell-mode
  :config
  (progn
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)))

(use-package flyspell-config
  :ensure nil
  :after haskell-mode
  :config (add-hook 'haskell-mode-hook 'flyspell-prog-mode))

(provide 'haskell-config)
